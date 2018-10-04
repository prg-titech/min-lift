package pass

import ast._

class Normalizer extends Visitor[Unit, Expression] {
  override def visit(node: Expression.Apply, a: Unit): ResultType = {
    node.callee match {
      // (o f g x) -> (f (g x))
      case Expression.Identifier("o", false) => {
        val f = node.args(0)
        val g = node.args(1)
        val arg = node.args(2)
        val newApply = Expression.Apply(f, List(Expression.Apply(g, List(arg))))
        newApply.accept(this, ())
      }
      // (((f x) y) z) -> ((f x) y z) -> (f x y z)
      case Expression.Apply(ccallee, cargs) => {
        val newApply = Expression.Apply(ccallee, cargs ++ node.args)
        newApply.accept(this, ())
      }
      case _ => {
        Expression.Apply(
          node.callee.accept(this, ()),
          node.args.map(_.accept(this, ())))
      }
    }
  }

  override def visit(node: Expression.Lambda, a: Unit): ResultType = {
    Expression.Lambda(node.args, node.body.accept(this, ()))
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

object Normalizer {
  def normalize(node: Lift) = {
    val norm = new Normalizer
    Lift(node.variables, node.inputTypes, node.body.accept(norm, ()).asInstanceOf[Expression.Lambda])
  }
}
