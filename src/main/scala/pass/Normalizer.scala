package pass

import ast._

class Normalizer extends Visitor[Unit, Node] {
  type Value = Node
  type NormalizerResult = Value

  override def visit(node: Lift, a: Unit): NormalizerResult = {
    Lift(node.variables, node.inputTypes, node.body.accept(this, ()).asInstanceOf[Expression])
  }

  override def visit(node: Expression.Apply, a: Unit): NormalizerResult = {
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

  override def visit(node: Expression.Lambda, a: Unit): NormalizerResult = {
    Expression.Lambda(node.args, node.body.accept(this, ()).asInstanceOf[Expression])
  }

  override def visit(node: Expression.Map, a: Unit): NormalizerResult = ???

  override def visit(node: Expression.Identifier, a: Unit): NormalizerResult = {
    node
  }

  override def visit[C](node: Expression.Const[C], a: Unit): NormalizerResult = {
    node
  }

  override def visit(node: Expression.Undefined, a: Unit): NormalizerResult = {
    node
  }
}

object Normalizer {
  def normalize(node: Lift) = (new Normalizer).visit(node, ()).asInstanceOf[Lift]
}
