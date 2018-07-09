package ast

class AstPrinter extends Visitor[String] {
  def pad(str: String): String = {
    str.split("\n").map(l => s"  ${l}").mkString("\n")
  }

  override def visit(node: Lift): String = {
    s"""
       |(lift
       |  (${node.variables.mkString(", ")})
       |  (${node.inputTypes.mkString(", ")})${pad(node.body.accept(this))})""".stripMargin
  }

  override def visit[U](node: Expression.Apply[U]): String = {
    val args = node.args.map {
      case arg:Expression => arg.accept(this)
      case arg:Type => arg.toString
    }.mkString(" ")
    s"""
       |(${node.callee.accept(this)}${pad(args)}): ${node.ty}""".stripMargin
  }

  override def visit(node: Expression.Lambda): String = {
    s"""
       |(lambda
       |  (${node.args.map(_.accept(this)).mkString(" ")})${pad(node.body.accept(this))}): ${node.ty}""".stripMargin
  }

  override def visit(node: Expression.Map): String = ???

  override def visit(node: Expression.Identifier): String = {
    s"${node.value}: ${node.ty}@${node.addressSpace}"
  }

  override def visit[U](node: Expression.Const[U]): String = {
    s"${node.value}: ${node.ty}@${node.addressSpace}"
  }

  override def visit(node: Expression.Undefined): String = {
    "undefined"
  }
}

object AstPrinter {
  def print(node: Lift) = (new AstPrinter).visit(node)
}
