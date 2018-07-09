package ast

trait Visitor[T] {
  def visit(node: Lift): T
  def visit[U](node: Expression.Apply): T
  def visit(node: Expression.Lambda): T
  def visit(node: Expression.Map): T
  def visit(node: Expression.Identifier): T
  def visit[U](node: Expression.Const[U]): T
  def visit(node: Expression.Undefined): T
}
