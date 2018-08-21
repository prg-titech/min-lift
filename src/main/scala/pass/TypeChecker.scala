package pass

import ast._
import ast.Type._
import ast.Expression._

class TypeInferer extends Visitor[Env, Either[String, (Type, Subst)]] {
  // for identical typeVar
  var varCount = 0

  def createTypeVar() : TypeVar = {
    val t = TypeVar(s"t${varCount}")
    varCount += 1
    t
  }

  override def visit(lift: Lift, _env: ArgumentType): ResultType = {
    val Lambda(args, _) = lift.body

    val lambdaEnv = args.zip(lift.inputTypes).map { case (id, ty) =>
        id.value -> TypeScheme(List(), ty)
    }

    val a = createTypeVar()
    val b = createTypeVar()
    val c = createTypeVar()

    var env = _env.append(Map(
      ("mapSeq" -> TypeScheme(List(a, b), (a ->: b) ->: Array(a) ->: Array(b))),
      ("o" -> TypeScheme(List(a, b, c), (b ->: c) ->: (a ->: b) ->: (a ->: c))),
      ("*" -> TypeScheme(List(), Float ->: Float ->: Float)),
      ("+" -> TypeScheme(List(), Float ->: Float ->: Float))))
      .append(lambdaEnv.toMap)

    Apply(lift.body, args).accept(this, env)
  }

  override def visit(lambda: Lambda, env: ArgumentType): ResultType = {
    val Lambda(args, body) = lambda
    val lambdaEnv = args.map(arg => {
      arg.value -> TypeScheme(List(), createTypeVar())
    })

    body.accept(this, env.append(lambdaEnv.toMap)).map { case (ty, subst) =>
      val lambdaType = lambdaEnv.foldRight(ty)((argTy, ty) => {
        argTy._2.toType(this) ->: ty
      })
      lambda.args.zip(lambdaEnv).foreach { case (arg, e) => arg.ty = Some(e._2.toType(this)) }
      lambda.ty = Some(lambdaType)
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
        apply.ty = Some(resultType)
        (resultType, subst1.concat(argSubst).append(ty1, calleeType))
      })
    }
  }

  override def visit(id: Identifier, env: ArgumentType): ResultType = {
    env.lookup(id.value).toRight(s"undefined identifier ${id.value}")
        .map(ty => {
          val idType  = ty.toType(this)
          id.ty = Some(idType)
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
      const.ty = Some(ty)
      Right(ty, EmptySubst())
    }
  }
}

class TypeReplacer(val subst: Subst) extends Visitor[Unit, Unit] {
  override def visit(node: Lift, arg: Unit): Unit = {
    node.body.accept(this, ())
  }

  override def visit(node: Apply, arg: Unit): Unit = {
    node.callee.accept(this, ())
    node.args.foreach(_.accept(this, ()))

    node.ty = node.ty.map(subst.replace(_))
  }

  override def visit(node: Lambda, arg: Unit): Unit = {
    node.args.foreach(_.accept(this, ()))
    node.body.accept(this, ())

    node.ty = node.ty.map(subst.replace(_))
  }

  override def visit(node: Identifier, arg: Unit): Unit = {
    node.ty = node.ty.map(subst.replace(_))
  }

  override def visit[C](node: Const[C], arg: Unit): Unit = {
  }
}

object TypeChecker {
  def unify(subst: Subst): Either[String, Subst] = {
    subst match {
      case SubstCons(ty1, ty2@TypeVar(_), next) => {
        if (ty1 == ty2) {
          unify(next)
        }
        else if (ty1.hasTypeVar(ty2)) {
          Left(s"$ty1 and $ty2: circular constraints")
        }
        else {
          unify(next.replaceBy(ty2, ty1)).map(_.append(ty2, ty1))
        }
      }
      case SubstCons(ty1@TypeVar(_), ty2, next) => {
        if (ty1 == ty2) {
          unify(next)
        }
        else if (ty2.hasTypeVar(ty1)) {
          Left(s"$ty1 and $ty2: circular constraints")
        }
        else {
          unify(next.replaceBy(ty1, ty2)).map(_.append(ty1, ty2))
        }
      }
      case SubstCons(ty1@Scalar(_), ty2@Scalar(_), next) => {
        if (ty1 == ty2) {
          unify(next)
        }
        else {
          Left(s"$ty1 and $ty2: unsolvable constraints")
        }
      }
      case SubstCons(Arrow(tyS1, tyS2), Arrow(tyT1, tyT2), next) => {
        unify(next.append(tyS1, tyT1).append(tyS2, tyT2))
      }
      case SubstCons(ty1@TypeCon(name1, it1), ty2@TypeCon(name2, it2), next) => {
        if (name1 == name2) {
          val itSubst = it1.zip(it2).foldRight(next)((ty, subst) => {
            subst.append(ty._1, ty._2)
          })
          unify(itSubst)
        }
        else {
          Left(s"$ty1 and $ty2: unsolvable constraints")
        }
      }
      case SubstCons(ty1, ty2, next) => {
        Left(s"$ty1 and $ty2: unsolvable constraints")
      }
      case EmptySubst() => {
        Right(EmptySubst())
      }
    }
  }

  def check(lift: Lift) = {
    val res = new TypeInferer().visit(lift, EmptyEnv())
    println(AstPrinter.print(lift))
    res.flatMap { case (ty, subst) => {
      val unifyed = unify(subst)
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

// Identifier (as String) -> TypeScheme
sealed trait Env {
  def lookup(id: String): Option[TypeScheme]

  def append(mapper: Map[String, TypeScheme]) = EnvCons(mapper, this)
}
case class EnvCons(val mapper: Map[String, TypeScheme], val next: Env) extends Env {
  def lookup(id: String): Option[TypeScheme] = {
    mapper.get(id) match {
      case ts@Some(_) => ts
      case None => next.lookup(id)
    }
  }
}
case class EmptyEnv() extends Env {
  def lookup(id: String): Option[TypeScheme] = None
}

sealed trait Subst {
  def lookup(x: TypeVar): Type
  def toString: String

  def replace(ty: Type): Type
  def replace(env: Env): Env
  def replaceBy(from: TypeVar, to: Type): Subst

  def append(t1: Type, t2: Type) = SubstCons(t1, t2, this)
  def concat(subst: Subst): Subst = subst match {
    case SubstCons(typeVar, ty, next) => append(typeVar, ty).concat(next)
    case EmptySubst() => this
  }
}

// Type -> Type
case class SubstCons(val t1: Type, val t2: Type, val next: Subst) extends Subst {
  def replace(ty: Type): Type = ty match {
    case tv@TypeVar(name) => {
      val u = lookup(tv)
      if (ty == u) { ty } else { replace(u) }
    }
    case Arrow(argType, resultType) => {
      Arrow(replace(argType), replace(resultType))
    }
    case TypeCon(name, innerTypes) => {
      TypeCon(name, innerTypes.map(replace))
    }
    case Scalar(_) => ty
  }

  def replace(env: Env): Env = env match {
    case EnvCons(mapper, next) => {
      EnvCons(mapper.mapValues(ts => ts match {
        case TypeScheme(typeVars, ty) => {
          TypeScheme(typeVars, replace(ty))
        }
      }), replace(next))
    }
    case EmptyEnv() => EmptyEnv()
  }

  def replaceBy(from: TypeVar, to: Type): Subst = {
    SubstCons(t1, t2.replaceBy(from, to), next.replaceBy(from, to))
  }

  def lookup(x: TypeVar): Type = {
    if (x == t1) t2
    else next.lookup(x)
  }
  override def toString: String = s"$t1 = $t2, " + next.toString
}

case class EmptySubst() extends Subst {
  def lookup(x: TypeVar): Type = x
  override def toString: String = "(empty)"

  def replace(ty: Type): Type = ty
  def replace(env: Env): Env = env
  def replaceBy(from: TypeVar, to: Type): Subst = this
}
