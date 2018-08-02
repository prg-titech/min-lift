package pass

import ast._
import ast.Type._

class TypeChecker extends Visitor[Either[String, ???]] {
  // for identical typeVar
  var varCount = 0

  def createTypeVar() : Type = {
    val t = TypeVar(s"${varCount}")
    varCount += 1
    t
  }


}

// This expresses forall(∀) and is root of type tree.
case class TypeScheme(val typeVars: List[TypeVar], val ty: Type) {
  // newInstance
  def toType(checker: TypeChecker): Type = {
  }

  override def toString: String = s"∀(${typeVars.mkString(", ")}) . (${ty})"
}

sealed abstract class SubstBase {
  def lookup(x: TypeVar): Type
  def toString: String
}

// TypeVar -> Type
case class Subst(val typeVar: TypeVar, val ty: Type, val next: SubstBase) extends SubstBase {

  // Identifier (as String) -> TypeScheme
  type Env = Map[String, TypeScheme]

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

  def replace(env: Env): Env = {
    env.mapValues(ts => ts match {
      case TypeScheme(typeVars, ty) => {
        TypeScheme(typeVars, replace(ty))
      }
    })
  }

  override def lookup(x: TypeVar): Type = {
    if (x == typeVar) ty
    else next.lookup(x)
  }
  override def toString: String = s"$typeVar = $ty, " + next.toString
}

case class EmptySubst() extends SubstBase {
  override def lookup(x: TypeVar): Type = x
  override def toString: String = "(empty)"
}
