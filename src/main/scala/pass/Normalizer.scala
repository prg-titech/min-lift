package pass

import ast._

class Normalizer extends Visitor[Node] {
  type Value = Node
  type NormalizerResult = Value

  override def visit(node: Lift): NormalizerResult = {
    Lift(node.variables, node.inputTypes, node.body.accept(this).asInstanceOf[Expression])
  }

  override def visit[U](node: Expression.Apply): NormalizerResult = {
    node.callee.ty match {
      case Type.Function(argTypes, resultType) => {
        if (argTypes.length == 0) {
          node.callee
        }
        else {
          val Expression.Apply(callee, args) = node.callee
          val newApply = Expression.Apply(callee, args ++ node.args)
          newApply.ty = Type.Function(argTypes.drop(node.args.length), resultType)
          newApply
        }
      }
    }
  }

  override def visit(node: Expression.Lambda): NormalizerResult = {
    Expression.Lambda(node.args, node.body.accept(this).asInstanceOf[Expression])
  }

  override def visit(node: Expression.Map): NormalizerResult = ???

  override def visit(node: Expression.Identifier): NormalizerResult = {
    node
  }

  override def visit[U](node: Expression.Const[U]): NormalizerResult = {
    node
  }

  override def visit(node: Expression.Undefined): NormalizerResult = {
    node
  }
}

object Normalizer {
  def normalize(node: Lift) = (new Normalizer).visit(node).asInstanceOf[Lift]
}
