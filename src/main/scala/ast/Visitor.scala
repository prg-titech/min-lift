package ast

trait Visitor[A, R] {
  def visit(node: Lift, arg: A): R
  def visit(node: Expression.Apply, arg: A): R
  def visit(node: Expression.Lambda, arg: A): R
  def visit(node: Expression.Map, arg: A): R
  def visit(node: Expression.Identifier, arg: A): R
  def visit[C](node: Expression.Const[C], arg: A): R
  def visit(node: Expression.Undefined, arg: A): R
}
