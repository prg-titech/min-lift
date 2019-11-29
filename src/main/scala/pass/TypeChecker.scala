package pass

import ast._
import ast.Type._
import ast.Expression._
import lib.ListOfEitherTransposer
import lib._
import errors._

class TypeInferer(val idGen: UniqueIdGenerator) extends ExpressionVisitor[Environment[TypeScheme], Either[LiftError, (Type, Subst)]] {

  def generateTypeVar(idGen: UniqueIdGenerator) = TypeVar(s"t${idGen.generateInt()}")

  def initEnv = lift.BuiltInFunctions.getFuncs(idGen)

  def visit(lift: Lift, _env: ArgumentType): ResultType = {
    val Lambda(args, _) = lift.body

    val lambdaEnv = args.zip(lift.inputTypes).map { case (id, ty) =>
        id.value -> TypeScheme(List(), ty)
    }

    var env = _env.mergeEnv(initEnv).pushEnv(lambdaEnv.toMap)

    Apply(lift.body, args).accept(this, env)
  }

  override def visit(lambda: Lambda, env: ArgumentType): ResultType = {
    val Lambda(args, body) = lambda
    val lambdaEnv = args.map(arg => {
      arg.value -> TypeScheme(List(), generateTypeVar(idGen))
    })

    body.accept(this, env.pushEnv(lambdaEnv.toMap)).map { case (ty, subst) =>
      val lambdaType = lambdaEnv.foldRight(ty)((argTy, ty) => {
        argTy._2.toType(idGen) ->: ty
      })
      lambda.args.zip(lambdaEnv).foreach { case (arg, e) => arg.ty = e._2.toType(idGen) }
      lambda.ty = lambdaType
      (lambdaType, subst)
    }
  }

  override def visit(let: Let, env: ArgumentType): ResultType = {
    let.value.accept(this, env).flatMap { case (valueType0, valueSubst) =>

      TypeChecker.unify(valueSubst, EmptySubst()).flatMap(unified => {
        val valueType = unified.replace(valueType0)(idGen)
        valueType match {
          case Existential(ty) => {
            val existSize = SizeVariable(s"l${idGen.generateInt()}")
            val existType = Existential(Array(generateTypeVar(idGen), existSize))

            let.body.accept(this, env.pushEnv(Map(let.id.value -> TypeScheme(List(), existType.ty)))).flatMap { case (bodyType, bodySubst) =>
              val subst = valueSubst.concat(bodySubst)

              TypeChecker.unify(subst, EmptySubst()).flatMap(unified => {
                val replacedTy = unified.replace(bodyType)(idGen)
                if (replacedTy.hasType(existSize)) {
                  val newBodyType = Existential(replacedTy)
                  let.ty = newBodyType
                  Right((bodyType, subst.append(valueType0, existType)))
                  // Left(TypeError("result type of unpack has its size variable"))
                }
                else {
                  let.ty = bodyType
                  Right((bodyType, subst.append(valueType0, existType)))
                }
              })
            }
          }
          case _ => {
            let.body.accept(this, env.pushEnv(Map(let.id.value -> TypeScheme(List(), valueType0)))).map { case (ty, subst) =>
              let.ty = ty
              (ty, valueSubst.concat(subst))
            }
          }
        }
      })
    }
  }

  override def visit(pack: Pack, env: ArgumentType): ResultType = {
    pack.value.accept(this, env).flatMap { case (valueType, valueSubst) =>
      TypeChecker.unify(valueSubst, EmptySubst()).flatMap(unified => {
        val replacedTy = unified.replace(valueType)(idGen)
        replacedTy match {
          case Array(innerType, size) => {
            val existType = Existential(Array(innerType, size))

            pack.ty = existType

            Right((existType, valueSubst))
          }
          case _ => {
            Left(TypeError("value of pack must be array"))
          }
        }
      })
    }
  }

  override def visit(apply: Apply, env: ArgumentType): ResultType = {
    apply.callee.accept(this, env).flatMap { case (ty1, subst1) =>
      val args = ListOfEitherTransposer.transpose(apply.args.map(_.accept(this, env)))

      args.map(args => {
        val resultType = generateTypeVar(idGen)
        val argSubst = args.foldRight(EmptySubst(): Subst)((arg, subst) =>
          subst.concat(arg._2)
        )
        val calleeType = args.foldRight(resultType: Type)((arg, ty) =>
          arg._1 ->: ty
        )
        apply.ty = resultType
        (resultType, subst1.concat(argSubst).append(ty1, calleeType))
      })
    }
  }

  override def visit(id: Identifier, env: ArgumentType): ResultType = {
    env.lookup(id.value).toRight(TypeError(s"undefined identifier ${id.value}"))
        .map(ty => {
          val idType  = ty.toType(idGen)
          id.ty = idType
          (idType, EmptySubst())
        })
  }

  override def visit[C](const: Const[C], env: ArgumentType): ResultType = const match {
    case Const(value) => {
      val ty = value match {
        case v:Float => Float
        case v:Double => Double
        case v:Int => Int
        case v:Boolean => Boolean
      }
      const.ty = ty
      Right(ty, EmptySubst())
    }
  }

  override def visit(node: Expression.Size, env: ArgumentType): ResultType = {
    Right(Type.SizeConst(node.value), EmptySubst())
  }
}

class TypeReplacer(val subst: Subst) extends ExpressionVisitor[UniqueIdGenerator, Unit] {
  def visit(node: Lift, arg: ArgumentType): Unit = {
    node.body.accept(this, arg)
  }

  override def visit(node: Apply, arg: ArgumentType): Unit = {
    node.callee.accept(this, arg)
    node.args.foreach(_.accept(this, arg))

    node.ty = subst.replace(node.ty)(arg)
  }

  override def visit(node: Lambda, arg: ArgumentType): Unit = {
    node.args.foreach(_.accept(this, arg))
    node.body.accept(this, arg)

    node.ty = subst.replace(node.ty)(arg)
  }

  override def visit(node: Let, arg: ArgumentType): Unit = {
    node.value.accept(this, arg)
    node.body.accept(this, arg)

    node.ty = subst.replace(node.ty)(arg)
  }

  override def visit(node: Pack, arg: ArgumentType): Unit = {
    node.value.accept(this, arg)

    node.ty = subst.replace(node.ty)(arg)
  }

  override def visit(node: Identifier, arg: ArgumentType): Unit = {
    node.ty = subst.replace(node.ty)(arg)
  }

  override def visit[C](node: Const[C], arg: ArgumentType): Unit = {
  }

  override def visit(node: Expression.Size, arg: ArgumentType): Unit = {
  }
}

object TypeChecker {
  def unify(subst: Subst, result: Subst): Either[LiftError, Subst] = {
//    subst match {
//      case SubstCons(t1, t2, next) => {
//        println(s"unifying $t1 and $t2")
//      }
//      case _ => {}
//    }

    subst match {
      case SubstCons(ty1@TypeVar(_), ty2, next) => {
        if (ty1 == ty2) {
          unify(next, result)
        }
        else if (ty2.hasType(ty1)) {
          Left(TypeError(s"$ty1 and $ty2: circular constraints"))
        }
        else {
          unify(next.replaceBy(ty1, ty2), result.replaceBy(ty1, ty2).append(ty1, ty2))
        }
      }
      case SubstCons(ty1, ty2@TypeVar(_), next) => {
        unify(next.append(ty2, ty1), result)
      }
      case SubstCons(ty1@Scalar(_), ty2@Scalar(_), next) => {
        if (ty1 == ty2) {
          unify(next, result)
        }
        else {
          Left(TypeError(s"$ty1 and $ty2: unsolvable constraints"))
        }
      }
      case SubstCons(Arrow(tyS1, tyS2), Arrow(tyT1, tyT2), next) => {
        unify(next.append(tyS1, tyT1).append(tyS2, tyT2), result)
      }
      case SubstCons(ty1@TypeCon(name1, it1), ty2@TypeCon(name2, it2), next) => {
        if (name1 == name2) {
          val itSubst = it1.zip(it2).foldRight(next)((ty, subst) => {
            subst.append(ty._1, ty._2)
          })
          unify(itSubst, result)
        }
        else {
          Left(TypeError(s"$ty1 and $ty2: unsolvable constraints"))
        }
      }
      case SubstCons(Array(it1, size1), Array(it2, size2), next) => {
        unify(next.append(it1, it2).append(size1, size2), result)
      }
      case SubstCons(Tuple2(fst1, snd1), Tuple2(fst2, snd2), next) => {
        unify(next.append(fst1, fst2).append(snd1, snd2), result)
      }
      case SubstCons(SizeBinaryOperator(a1, b1), SizeBinaryOperator(a2, b2), next) => {
        unify(next.append(a1, b1).append(a2, b2), result)
      }
      case SubstCons(Existential(ty1), Existential(ty2), next) => {
        unify(next.append(ty1, ty2), result)
      }
      case SubstCons(ty1, ty2, next) => {
        if (ty1 == ty2) {
          unify(next, result)
        }
        else {
          Left(TypeError(s"$ty1 and $ty2: unsolvable constraints"))
        }
      }
      case EmptySubst() => {
        Right(result)
      }
    }
  }

  def check(lift: Lift) = {
    val idGen = new UniqueIdGenerator
    val inferer = new TypeInferer(idGen)

    val res = inferer.visit(lift, EmptyEnvironment[TypeScheme]())
    // println(AstPrinter.print(lift))
    res.flatMap { case (ty, subst) => {
      val unifyed = unify(subst, EmptySubst())
      // println(subst, "\n", unifyed)
      unifyed.map((unifyed) => {
        new TypeReplacer(unifyed).visit(lift, idGen)
        lift
      })
    }}
  }
}

// This expresses forall(∀) and is root of type tree.
case class TypeScheme(val typeVars: List[TypeVar], val ty: Type) {
  // create identical type
  def toType(idGen: UniqueIdGenerator): Type = {
    val subst = typeVars.foldRight(EmptySubst() : Subst){ case (typeVar, subst) =>
      SubstCons(typeVar, TypeVar(s"t${idGen.generateInt()}"), subst)
    }
    subst.replace(ty)(idGen)
  }

  override def toString: String = s"∀(${typeVars.mkString(", ")}) . (${ty})"
}

sealed trait Subst {
  def lookup(x: TypeVar): Type
  def lookupByValue(x: Type): Type
  def toString: String

  def replace(ty: Type)(implicit idGen: UniqueIdGenerator): Type
  def replace(env: Environment[TypeScheme])(implicit idGen: UniqueIdGenerator): Environment[TypeScheme]
  def replaceBy(from: TypeVar, to: Type): Subst

  def append(t1: Type, t2: Type) = {
    SubstCons(t1, t2, this)
  }
  def concat(subst: Subst): Subst = subst match {
    case SubstCons(typeVar, ty, next) => append(typeVar, ty).concat(next)
    case EmptySubst() => this
  }

  def swapTypesIfValueIs(ty: Type): Subst
}

// Type -> Type
case class SubstCons(val t1: Type, val t2: Type, val next: Subst) extends Subst {
  def replace(ty: Type)(implicit idGen: UniqueIdGenerator): Type = ty match {
    case tv@TypeVar(name) => {
      val u = lookup(tv)
      if (tv == u) {
        tv
      }
      else {
        replace(u)
      }
    }
    case Arrow(argType, resultType) => {
      Arrow(replace(argType), replace(resultType))
    }
    case TypeCon(name, innerTypes) => {
      TypeCon(name, innerTypes.map(replace))
    }
    case Scalar(_) => ty
    case Array(it, size) => {
      Array(replace(it), replace(size))
    }
    case Tuple2(fst, snd) => {
      Tuple2(replace(fst), replace(snd))
    }
    case SizeVariable(_) => ty
    case SizeDivision(dd, dr) => SizeDivision(replace(dd), replace(dr))
    case SizeMultiply(x, y)   => SizeMultiply(replace(x), replace(y))
    case SizeConst(_) => ty
    case Existential(ty) => Existential(replace(ty))
  }

  def replace(env: Environment[TypeScheme])(implicit idGen: UniqueIdGenerator): Environment[TypeScheme] = env match {
    case ConsEnvironment(mapper, next) => {
      ConsEnvironment(mapper.mapValues(ts => ts match {
        case TypeScheme(typeVars, ty) => {
          TypeScheme(typeVars, replace(ty))
        }
      }), replace(next))
    }
    case EmptyEnvironment() => EmptyEnvironment()
  }

  def replaceBy(from: TypeVar, to: Type): Subst = {
    SubstCons(t1.replaceBy(from, to), t2.replaceBy(from, to), next.replaceBy(from, to))
  }

  def lookup(x: TypeVar): Type = {
    if (x == t1) t2
    else next.lookup(x)
  }
  def lookupByValue(x: Type): Type = {
    if (x == t2) t1
    else next.lookupByValue(x)
  }

  override def toString: String = s"\t$t1 = $t2, \n" + next.toString

  def swapTypesIfValueIs(ty: Type): Subst = {
    if (t2 == ty) {
      SubstCons(t2, t1, next.swapTypesIfValueIs(ty))
    }
    else {
      SubstCons(t1, t2, next.swapTypesIfValueIs(ty))
    }
  }
}

case class EmptySubst() extends Subst {
  def lookup(x: TypeVar): Type = x
  def lookupByValue(x: Type): Type = x
  override def toString: String = "\t(empty)"

  def replace(ty: Type)(implicit idGen: UniqueIdGenerator): Type = ty
  def replace(env: Environment[TypeScheme])(implicit idGen: UniqueIdGenerator) = env
  def replaceBy(from: TypeVar, to: Type): Subst = this

  def swapTypesIfValueIs(ty: Type): Subst = this
}
