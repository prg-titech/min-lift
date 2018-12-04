package pass

import ast._
import ast.Type._
import ast.Expression._
import structures._

class TypeInferer extends ExpressionVisitor[Environment[TypeScheme], Either[String, (Type, Subst)]] {
  // for identical typeVar
  var varCount = 0

  def createTypeVar() : TypeVar = {
    val t = TypeVar(s"t${varCount}")
    varCount += 1
    t
  }

  def visit(lift: Lift, _env: ArgumentType): ResultType = {
    val Lambda(args, _) = lift.body

    val lambdaEnv = args.zip(lift.inputTypes).map { case (id, ty) =>
        id.value -> TypeScheme(List(), ty)
    }

    val a = createTypeVar()
    val b = createTypeVar()
    val c = createTypeVar()

    var env = _env.pushEnv(Map(
      ("mapSeq" -> TypeScheme(List(a, b, c), (a ->: b) ->: Array(a, c) ->: Array(b, c))),
      ("mapGlb" -> TypeScheme(List(a, b, c), (a ->: b) ->: Array(a, c) ->: Array(b, c))),
      ("reduceSeq" -> TypeScheme(List(a, b, c), b ->: (b ->: a ->: b) ->: Array(a, c) ->: Array(b, SizeConst(1)))),
      ("split" -> TypeScheme(List(a, b, c), a ->: Array(b, c) ->: Array(Array(b, a), SizeDivision(c, a)))),
      ("join" -> TypeScheme(List(a, b, c), Array(Array(a, b), c) ->: Array(a, SizeMultiply(b, c)))),
//      ("toGlobal" -> TypeScheme(List(a, b), (a ->: b) ->: (a ->: b))),
//      ("toLocal" -> TypeScheme(List(a, b), (a ->: b) ->: (a ->: b))),
//      ("toPrivate" -> TypeScheme(List(a, b), (a ->: b) ->: (a ->: b))),
      ("toGlobal" -> TypeScheme(List(a), a ->: a)),
      ("toLocal" -> TypeScheme(List(a), a ->: a)),
      ("toPrivate" -> TypeScheme(List(a), a ->: a)),
      ("o" -> TypeScheme(List(a, b, c), (b ->: c) ->: (a ->: b) ->: (a ->: c))),
      ("*" -> TypeScheme(List(), Float ->: Float ->: Float)),
      ("+" -> TypeScheme(List(), Float ->: Float ->: Float))))
      .pushEnv(lambdaEnv.toMap)

    Apply(lift.body, args).accept(this, env)
  }

  override def visit(lambda: Lambda, env: ArgumentType): ResultType = {
    val Lambda(args, body) = lambda
    val lambdaEnv = args.map(arg => {
      arg.value -> TypeScheme(List(), createTypeVar())
    })

    body.accept(this, env.pushEnv(lambdaEnv.toMap)).map { case (ty, subst) =>
      val lambdaType = lambdaEnv.foldRight(ty)((argTy, ty) => {
        argTy._2.toType(this) ->: ty
      })
      lambda.args.zip(lambdaEnv).foreach { case (arg, e) => arg.ty = e._2.toType(this) }
      lambda.ty = lambdaType
      (lambdaType, subst)
    }
  }

  override def visit(apply: Apply, env: ArgumentType): ResultType = {
    apply.callee.accept(this, env).flatMap { case (ty1, subst1) =>
      // convert List[Either[E, X]] to Either[E, List[X]]
      val args = apply.args
          .map(_.accept(this, env))
          .foldLeft(Right(List()): Either[String, List[(Type, Subst)]])((args, arg) =>
            arg match {
              case Right(t) => args.map(t :: _)
              case Left(err) => Left(err)
            }
          )

      args.map(args => {
        val resultType = createTypeVar()
        val argSubst = args.foldRight(EmptySubst(): Subst)((arg, subst) =>
          subst.concat(arg._2)
        )
        val calleeType = args.reverse.foldRight(resultType: Type)((arg, ty) =>
          arg._1 ->: ty
        )
        apply.ty = resultType
        (resultType, subst1.concat(argSubst).append(ty1, calleeType))
      })
    }
  }

  override def visit(id: Identifier, env: ArgumentType): ResultType = {
    env.lookup(id.value).toRight(s"undefined identifier ${id.value}")
        .map(ty => {
          val idType  = ty.toType(this)
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
      }
      const.ty = ty
      Right(ty, EmptySubst())
    }
  }

  override def visit(node: Expression.Size, env: ArgumentType): ResultType = {
    Right(Type.SizeConst(node.value), EmptySubst())
  }
}

class TypeReplacer(val subst: Subst) extends ExpressionVisitor[Unit, Unit] {
  def visit(node: Lift, arg: Unit): Unit = {
    node.body.accept(this, ())
  }

  override def visit(node: Apply, arg: Unit): Unit = {
    node.callee.accept(this, ())
    node.args.foreach(_.accept(this, ()))

    node.ty = subst.replace(node.ty)
  }

  override def visit(node: Lambda, arg: Unit): Unit = {
    node.args.foreach(_.accept(this, ()))
    node.body.accept(this, ())

    node.ty = subst.replace(node.ty)
  }

  override def visit(node: Identifier, arg: Unit): Unit = {
    node.ty = subst.replace(node.ty)
  }

  override def visit[C](node: Const[C], arg: Unit): Unit = {
  }

  override def visit(node: Expression.Size, arg: Unit): Unit = {
  }
}

object TypeChecker {
  def unify(subst: Subst, result: Subst): Either[String, Subst] = {
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
        else if (ty2.hasTypeVar(ty1)) {
          Left(s"$ty1 and $ty2: circular constraints")
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
          Left(s"$ty1 and $ty2: unsolvable constraints")
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
          Left(s"$ty1 and $ty2: unsolvable constraints")
        }
      }
      case SubstCons(ty1@Array(it1, size1), ty2@Array(it2, size2), next) => {
        unify(next.append(it1, it2).append(size1, size2), result)
      }
      case SubstCons(ty1@SizeBinaryOperator(a1, b1), ty2@SizeBinaryOperator(a2, b2), next) => {
        unify(next.append(a1, b1).append(a2, b2), result)
      }
      case SubstCons(ty1, ty2, next) => {
        if (ty1 == ty2) {
          unify(next, result)
        }
        else {
          Left(s"$ty1 and $ty2: unsolvable constraints")
        }
      }
      case EmptySubst() => {
        Right(result)
      }
    }
  }

  def check(lift: Lift) = {
    val res = new TypeInferer().visit(lift, EmptyEnvironment[TypeScheme]())
//    println(AstPrinter.print(lift) + "\n")
    res.flatMap { case (ty, subst) => {
      val unifyed = unify(subst, EmptySubst())
//      println(unifyed)
      unifyed.map((unifyed) => {
        new TypeReplacer(unifyed).visit(lift, ())
        lift
      })
    }}
  }
}

// This expresses forall(∀) and is root of type tree.
case class TypeScheme(val typeVars: List[TypeVar], val ty: Type) {
  // create identical type
  def toType(inferer: TypeInferer): Type = {
    val subst = typeVars.foldRight(EmptySubst() : Subst){ case (typeVar, subst) =>
      SubstCons(typeVar, inferer.createTypeVar(), subst)
    }
    subst.replace(ty)
  }

  override def toString: String = s"∀(${typeVars.mkString(", ")}) . (${ty})"
}

sealed trait Subst {
  def lookup(x: TypeVar): Type
  def lookupByValue(x: Type): Type
  def toString: String

  def replace(ty: Type): Type
  def replace(env: Environment[TypeScheme]): Environment[TypeScheme]
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
  def replace(ty: Type): Type = ty match {
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
    case SizeVariable(_) => ty
    case SizeDivision(dd, dr) => SizeDivision(replace(dd), replace(dr))
    case SizeMultiply(x, y)   => SizeMultiply(replace(x), replace(y))
    case SizeConst(_) => ty
  }

  def replace(env: Environment[TypeScheme]): Environment[TypeScheme] = env match {
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

  def replace(ty: Type): Type = ty
  def replace(env: Environment[TypeScheme]) = env
  def replaceBy(from: TypeVar, to: Type): Subst = this

  def swapTypesIfValueIs(ty: Type): Subst = this
}
