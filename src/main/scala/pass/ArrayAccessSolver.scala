package pass

import ast._

class ArrayAccessSolver extends Visitor[List[View], View]{
  def visit(node: Lift, arg: ArgumentType): ResultType = {
    node.body.accept(this, List(NullView()))
  }

  override def visit(node: Expression.Apply, arg: ArgumentType): ResultType = {
    val args = node.args.map(_.accept(this, ))

  }

  override def visit(node: Expression.Lambda, arg: ArgumentType): ResultType = ???

  override def visit(node: Expression.Identifier, arg: ArgumentType): ResultType = ???

  override def visit[C](node: Expression.Const[C], arg: ArgumentType): ResultType = ???

  override def visit(node: Expression.Size, arg: ArgumentType): ResultType = ???
}

object ArrayAccessSolver {
  def solve(node: Lift) = (new ArrayAccessSolver).visit(node, List(NullView()))
}

sealed class View
case class ArrayAccessView(indexName: String) extends View
case class SplitView(size: Int) extends View
case class MemoryView(varName: String) extends View
case class NullView() extends View

sealed class ViewExpression
object ViewExpression {
  case class Variable(name: String) extends ViewExpression
  case class BinaryOp(left: ViewExpression, op: Operation, right: ViewExpression) extends ViewExpression
  case class IntLiteral(value: Int) extends ViewExpression

  class Operation(str: String)
  case object Plus extends Operation("+")
  case object Mult extends Operation("*")
}

