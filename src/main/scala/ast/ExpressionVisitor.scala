package ast

trait ExpressionVisitor[A, R] {
  type ArgumentType = A
  type ResultType = R

  def visit(node: Expression.Apply, arg: A): R
  def visit(node: Expression.Lambda, arg: A): R
  def visit(node: Expression.Let, arg: A): R
  def visit(node: Expression.Pack, arg: A): R
  def visit(node: Expression.Identifier, arg: A): R
  def visit[C](node: Expression.Const[C], arg: A): R
  def visit(node: Expression.Size, arg: A): R
}
