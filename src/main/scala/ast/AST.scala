package ast

// sealed abstract class Node

case class Lift(val variables: Vector[Type.Size], val inputTypes: Vector[Type], val body: Expression.Lambda)

sealed abstract class Type {
  def toCL: String

  def ->:(ty: Type): Type = {
    Type.Arrow(ty, this)
  }

  // check this has terminal type(Scalar, TypeVar and Size)
  def hasType(ty: Type): Boolean
  def replaceBy(from: Type.TypeVar, to: Type): Type

  def representativeType = this
}
object Type {
  case class TypeVar(val name: String) extends Type {
    override def toString: String = s"<${name}>"
    override def toCL: String = ???

    def hasType(ty: Type): Boolean = ty == this
    def replaceBy(from: Type.TypeVar, to: Type): Type = {
      if (this == from) to
      else this
    }
  }

  case class Arrow(val argType: Type, val resultType: Type) extends Type {
    override def toString: String = s"($argType => $resultType)"
    override def toCL: String = resultType.toCL // ?

    override def representativeType: Type = lastResultType

    def hasType(ty: Type): Boolean = {
      argType.hasType(ty) || resultType.hasType(ty)
    }
    def replaceBy(from: Type.TypeVar, to: Type): Type = {
      Arrow(argType.replaceBy(from, to), resultType.replaceBy(from, to))
    }

    def lastResultType: Type = resultType match {
      case ty@Arrow(_, _) => ty.lastResultType
      case _ => resultType
    }

    def nthArg(n: Int): Type = {
      if (n <= 0) {
        argType
      }
      else {
        resultType match {
          case ty@Arrow(_, _) => ty.nthArg(n - 1)
          case _ => resultType
        }
      }
    }

    def args: List[Type] = resultType match {
      case ty@Arrow(_, _) => argType :: ty.args
      case _ => List(argType)
    }
  }

  case class TypeCon(val name: String, val innerTypes: Vector[Type]) extends Type {
    override def toString: String = s"$name[${innerTypes.mkString(", ")}]"
    override def toCL: String = s"$name???"

    def hasType(ty: Type): Boolean = {
      innerTypes.exists(_.hasType(ty))
    }
    def replaceBy(from: Type.TypeVar, to: Type): Type = {
      TypeCon(name, innerTypes.map(_.replaceBy(from, to)))
    }
  }

  abstract class Scalar(val name: String) extends Type {
    override def toCL: String = name

    def hasType(ty: Type): Boolean = ty == this
    def replaceBy(from: Type.TypeVar, to: Type): Type = this
  }
  object Scalar {
    def unapply(arg: Scalar): Option[String] = Some(arg.name)
  }
  case object Float extends Scalar("float")
  case object Double extends Scalar("double")
  case object Int extends Scalar("int")
  case object Boolean extends Scalar("bool")

  case class Array(val innerType: Type, val size: Type) extends Type {
    override def toCL: String = {
      size match {
        // case SizeDynamicInstance(_) => s"dyn_ary_${innerType.toCL}"
        case _ => s"${innerType.toCL}*"
      }
    }

    override def hasType(ty: Type): Boolean = {
      innerType.hasType(ty) || size.hasType(ty)
    }
    override def replaceBy(from: TypeVar, to: Type): Type = {
      Array(innerType.replaceBy(from, to), size.replaceBy(from, to))
    }
  }

  case class Tuple2(fst: Type, snd: Type) extends Type {
    override def toCL: String = s"tuple2<${fst.toCL}, ${snd.toCL}>" // ???

    override def hasType(ty: Type): Boolean = {
      fst.hasType(ty) || snd.hasType(ty)
    }

    override def replaceBy(from: TypeVar, to: Type): Type = {
      Tuple2(fst.replaceBy(from, to), snd.replaceBy(from, to))
    }
  }

  abstract class Size extends Type {
    override def toString: String
    override def toCL: String
  }
  case class SizeVariable(val name: String) extends Size {
    override def toString: String = name
    override def toCL: String = name

    def hasType(ty: Type): Boolean = ty == this
    def replaceBy(from: TypeVar, to: Type): Type = this
  }
  case class SizeConst(val value: Int) extends Size {
    override def toString: String = s"#${value.toString}"
    override def toCL: String = value.toString

    def hasType(ty: Type): Boolean = ty == this
    def replaceBy(from: TypeVar, to: Type): Type = this
  }
  abstract class SizeBinaryOperator(val a: Type, val b: Type) extends Size {
    override def toCL: String = ???

    def hasType(ty: Type): Boolean = {
      a.hasType(ty) || b.hasType(ty)
    }
  }
  object SizeBinaryOperator {
    def unapply(size: SizeBinaryOperator): Option[(Type, Type)] = Some((size.a, size.b))
  }
  case class SizeDivision(dividend: Type, divisor: Type) extends SizeBinaryOperator(dividend, divisor) {
    override def toString: String = s"$a/$b"
    override def toCL: String = s"${a.toCL}/${b.toCL}"

    def replaceBy(from: TypeVar, to: Type): Type = {
      SizeDivision(a.replaceBy(from, to), b.replaceBy(from, to))
    }
  }
  case class SizeMultiply(x: Type, y: Type) extends SizeBinaryOperator(x, y) {
    override def toString: String = s"$a*$b"
    override def toCL: String = s"${a.toCL}*${b.toCL}"

    def replaceBy(from: TypeVar, to: Type): Type = {
      SizeMultiply(a.replaceBy(from, to), b.replaceBy(from, to))
    }
  }

  case class Existential(/*typeVar: TypeVar,*/ ty: Type) extends Type {
    override def toCL: String = ty.toCL
    override def hasType(tv: Type): Boolean = ty.hasType(tv)
    override def replaceBy(from: TypeVar, to: Type): Type = Existential(ty.replaceBy(from, to))
  }
}

sealed abstract class Expression {
  // FIXME: don't use var
  var ty: Type = null
  var addressSpace: Option[pass.MemoryAllocator.AddressSpace] = None
  var view: pass.View = null

  def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R
}
object Expression {
  case class Apply(val callee: Expression, val args: List[Expression]) extends Expression {
    def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Lambda(val args: List[Identifier], val body: Expression) extends Expression {
    def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Let(val id: Identifier, val value: Expression, val body: Expression, val unpack: Boolean) extends Expression {
    def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Pack(val value: Expression) extends Expression {
    def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Identifier(val value: String, val isParam: Boolean) extends Expression {
    def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }

  case class Const[U](val value: U) extends Expression {
    def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }

    def toCL: String = value match {
      case v:Float => s"${value}f"
      case v:Double => value.toString
      case v:Int => value.toString
    }
  }

  case class Size(val value: Int) extends Expression {
    def accept[A, R](visitor: ExpressionVisitor[A, R], arg: A): R = {
      visitor.visit(this, arg)
    }
  }
}
