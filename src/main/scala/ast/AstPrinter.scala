package ast

class AstPrinter extends Visitor[Unit, String] {
  def pad(str: String): String = {
    str.split("\n").map(l => s"  ${l}").mkString("\n")
  }

  def visit(node: Lift, a: Unit): String = {
    s"""
       |(lift
       |  (${node.variables.mkString(", ")})
       |  (${node.inputTypes.mkString(", ")})${pad(node.body.accept(this, ()))})""".stripMargin
  }

  override def visit(node: Expression.Apply, a: Unit): String = {
    val args = node.args.map(_.accept(this, ())).mkString("\n")
    s"""
       |(${node.callee.accept(this, ())}\n${pad(args)}):${node.ty}""".stripMargin
  }

  override def visit(node: Expression.Lambda, a: Unit): String = {
    s"""
       |(lambda
       |  (${node.args.map(_.accept(this, ())).mkString("\n")})${pad(node.body.accept(this, ()))}):${node.ty}""".stripMargin
  }

  override def visit(node: Expression.Identifier, a: Unit): String = {
    s"${node.value}:${node.ty}@${node.addressSpace}"
  }

  override def visit[C](node: Expression.Const[C], a: Unit): String = {
    s"${node.value}:${node.ty}@${node.addressSpace}"
  }

  override def visit(node: Expression.Size, a: Unit): String = {
    s"${node.value}:${node.ty}@${node.addressSpace}"
  }
}

object AstPrinter {
  def print(node: Lift) = {
    (new AstPrinter).visit(node, ())
  }
}
