package pass

import ast._

class TypedNormalizer extends Visitor[Unit, Expression] {
  override def visit(node: Expression.Apply, a: Unit): ResultType = {
    node
  }

  override def visit(node: Expression.Lambda, a: Unit): ResultType = {
    node
  }

  override def visit(node: Expression.Identifier, a: Unit): ResultType = {
    node
  }

  override def visit[C](node: Expression.Const[C], a: Unit): ResultType = {
    node
  }

  override def visit(node: Expression.Size, a: Unit): ResultType = {
    node
  }
}

object TypedNormalizer {
  def normalize(node: Lift) = {
    val norm = new TypedNormalizer
    Lift(node.variables, node.inputTypes, node.body.accept(norm, ()).asInstanceOf[Expression.Lambda])
  }
}
