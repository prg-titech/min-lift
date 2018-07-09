package ast

sealed abstract class Node

case class Lift(val variables: Vector[Variable], val inputTypes: Vector[Type], val body: Expression) extends Node {
  def accept[T](visitor: Visitor[T]): Unit = {
    visitor.visit(this)
  }
}

sealed abstract class Type {
  def toCL: String
}
object Type {
  case class Array(val innerType: Type, val length: Variable) extends Type {
    override def toString: String = s"${innerType}[${length}]"
    override def toCL: String = s"${innerType.toCL}*"
  }
  case class Function(val argTypes: Vector[Type], val resultType: Type) extends Type {
    override def toString: String = s"(${argTypes.mkString(", ")}) => ${resultType}"
    override def toCL: String = resultType.toCL // ?
  }
  abstract class Scalar extends Type
  case object Float extends Scalar {
    override def toCL: String = "float"
  }
  case object Double extends Scalar {
    override def toCL: String = "double"
  }
  case object Int extends Scalar {
    override def toCL: String = "int"
  }

  // TODO: Add vector type and tuple type

  case class Polymorphic(val name: String) extends Type {
    override def toString: String = s"<${name}>"
    override def toCL: String = ???
  }

  case object Unfixed extends Type {
    override def toCL: String = ???
  }
}

case class Variable(val name: String) {
  override def toString: String = name
}

sealed abstract class Expression extends Node {
  var ty: Type = Type.Unfixed
  var addressSpace: Option[pass.MemoryAllocator.AddressSpace] = None

  def accept[T](visitor: Visitor[T]): T
}
object Expression {
  case class Apply(val callee: Expression, val args: Vector[Expression]) extends Expression {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visit(this)
    }
  }

  case class Lambda(val args: Vector[Identifier], val body: Expression) extends Expression {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visit(this)
    }
  }

  case class Map(val f: Expression) extends Expression {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visit(this)
    }
  }

  case class Identifier(val value: String, val isParam: Boolean) extends Expression {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visit(this)
    }
  }

  case class Const[U](val value: U) extends Expression {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visit(this)
    }
  }

  case class Undefined() extends Expression {
    def accept[T](visitor: Visitor[T]): T = {
      visitor.visit(this)
    }
  }
  object Undefined {
    def withType(ty: Type): Undefined = {
      val undef = Undefined()
      undef.ty = ty
      undef
    }
  }
}
