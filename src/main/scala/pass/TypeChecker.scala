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
    val Lambda(args, body) = lift.body

    val lambdaEnv = args.zip(lift.inputTypes).map { case (id, ty) =>
        id.value -> TypeScheme(List(), ty)
    }

    val a = createTypeVar()
    val b = createTypeVar()
    var env = _env.append(Map(
      ("mapSeq" -> TypeScheme(List(a), (a ->: b) ->: Array(a) ->: Array(b))),
      ("*" -> TypeScheme(List(), Float ->: Float ->: Float))))
      .append(lambdaEnv.toMap)

    body.accept(this, env)
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
      (lambdaType, subst)
    }
  }

  override def visit(arrow: Arrow, env: ArgumentType): ResultType = {

  }

  override def visit(id: Identifier, env: ArgumentType): ResultType = {
    env.lookup(id.value).toRight(s"undefined identifier ${id.value}")
        .map(ty => {
          (ty.toType(this), createTypeVar(), EmptySubst())
        })
  }
}

object TypeInferer {
  def check(lift: Lift): [String, Unit] = {
    new TypeInferer().visit(lift, Map[String, TypeScheme]())
    ()
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
}

// TypeVar -> Type
case class SubstCons(val typeVar: TypeVar, val ty: Type, val next: Subst) extends Subst {
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

  def lookup(x: TypeVar): Type = {
    if (x == typeVar) ty
    else next.lookup(x)
  }
  override def toString: String = s"$typeVar = $ty, " + next.toString
}

case class EmptySubst() extends Subst {
  def lookup(x: TypeVar): Type = x
  override def toString: String = "(empty)"

  def replace(ty: Type): Type = ty
  def replace(env: Env): Env = env
}
