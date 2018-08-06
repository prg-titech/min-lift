package pass

import ast._

class Normalizer extends Visitor[Unit, Node] {
  type Value = Node
  type NormalizerResult = Value

  override def visit(node: Lift, a: Unit): NormalizerResult = {
    Lift(node.variables, node.inputTypes, node.body.accept(this, ()).asInstanceOf[Expression.Lambda])
  }

  // (((f x) y) z) -> ((f x) y z) -> (f x y z)
  override def visit(node: Expression.Apply, a: Unit): NormalizerResult = {
    node.callee match {
      case Expression.Apply(ccallee, cargs) => {
        val newApply = Expression.Apply(ccallee, cargs ++ node.args)
        newApply.accept(this, ())
      }
      case _ => {
        Expression.Apply(
          node.callee.accept(this, ()).asInstanceOf[Expression],
          node.args.map(_.accept(this, ()).asInstanceOf[Expression]))
      }
    }
  }

  override def visit(node: Expression.Lambda, a: Unit): NormalizerResult = {
    Expression.Lambda(node.args, node.body.accept(this, ()).asInstanceOf[Expression])
  }

  override def visit(node: Expression.Identifier, a: Unit): NormalizerResult = {
    node
  }

  override def visit[C](node: Expression.Const[C], a: Unit): NormalizerResult = {
    node
  }
}

object Normalizer {
  def normalize(node: Lift) = (new Normalizer).visit(node, ()).asInstanceOf[Lift]
}
