package ast

sealed abstract class Node

case class Lift(val variables: Vector[Variable], val inputTypes: Vector[Type], val body: Expression.Lambda) extends Node {
  def accept[A, R](visitor: Visitor[A, R], arg: A): R = {
    visitor.visit(this, arg)
  }
}

sealed abstract class Type {
  def toCL: String

  def ->:(ty: Type): Type = {
    Type.Arrow(ty, this)
  }

  def hasTypeVar(typeVar: Type.TypeVar): Boolean
  def replaceBy(from: Type.TypeVar, to: Type): Type
}
object Type {
  case class TypeVar(val name: String) extends Type {
    override def toString: String = s"<${name}>"
    override def toCL: String = ???

    def hasTypeVar(typeVar: Type.TypeVar): Boolean = typeVar == this
    def replaceBy(from: Type.TypeVar, to: Type): Type = {
      if (this == from) to
      else this
    }
  }

  case class Arrow(val argType: Type, val resultType: Type) extends Type {
    override def toString: String = s"($argType => $resultType)"
    override def toCL: String = resultType.toCL // ?

    def hasTypeVar(typeVar: Type.TypeVar): Boolean = {
      argType.hasTypeVar(typeVar) || resultType.hasTypeVar(typeVar)
    }
    def replaceBy(from: Type.TypeVar, to: Type): Type = {
      Arrow(argType.replaceBy(from, to), resultType.replaceBy(from, to))
    }
  }

  case class TypeCon(val name: String, innerTypes: Vector[Type]) extends Type {
    override def toString: String = s"$name[${innerTypes.mkString(", ")}]"
    override def toCL: String = s"$name???"

    def hasTypeVar(typeVar: Type.TypeVar): Boolean = {
      innerTypes.exists(_.hasTypeVar(typeVar))
    }
    def replaceBy(from: Type.TypeVar, to: Type): Type = {
      TypeCon(name, innerTypes.map(_.replaceBy(from, to)))
    }
  }

  abstract class Scalar(val name: String) extends Type {
    override def toCL: String = name

    def hasTypeVar(typeVar: TypeVar): Boolean = false
    def replaceBy(from: Type.TypeVar, to: Type): Type = this
  }
  object Scalar {
    def unapply(arg: Scalar): Option[String] = Some(arg.name)
  }
  case object Float extends Scalar("float")
  case object Double extends Scalar("double")
  case object Int extends Scalar("int")

  def Array(it: Type) = TypeCon("Array", Vector(it))
}

case class Variable(val name: String) {
  override def toString: String = name
}

sealed abstract class Expression extends Node {
  // FIXME: don't use var
  var ty: Option[Type] = None
  var addressSpace: Option[pass.MemoryAllocator.AddressSpace] = None

  def accept[A, R](visitor: Visitor[A, R], arg: A): R
}
object Expression {
  case class Apply(val callee: Expression, val args: List[Expression]) extends Expression {
    def accept[A, R](visitor: Visitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Lambda(val args: List[Identifier], val body: Expression) extends Expression {
    def accept[A, R](visitor: Visitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Identifier(val value: String, val isParam: Boolean) extends Expression {
    def accept[A, R](visitor: Visitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Const[U](val value: U) extends Expression {
    def accept[A, R](visitor: Visitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }
}
