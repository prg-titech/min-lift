package ast

import pass.ViewConstructor

class AstPrinter extends ExpressionVisitor[Unit, String] {
  def pad(str: String): String = {
    str.split("\n").map(l => s"  ${l}").mkString("\n")
  }

  def postfix(node: Expression) = {
    if (node.ty != null) {
      s"${node.ty}"
    }
    else {
      ""
    }
  }

  def view(node: Expression) = {
    if (node.view != null) {
      val v = ViewConstructor.construct(node.view)
      v.map(s => s"($s)").getOrElse("")
    }
    else {
      ""
    }
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
       |(${node.callee.accept(this, ())}\n${pad(args)}):${postfix(node)}${view(node)}""".stripMargin
  }

  override def visit(node: Expression.Lambda, a: Unit): String = {
    s"""
       |(lambda
       |  (${node.args.map(_.accept(this, ())).mkString("\n")})${pad(node.body.accept(this, ()))}):${postfix(node)}""".stripMargin
  }

  override def visit(node: Expression.Let, a: Unit): String = {
    val let = if (node.unpack) { "unpack" } else { "let" }
    s"""
       |(${let} ${node.id.accept(this, ())} ${node.value.accept(this, ())}
       |  ${pad(node.body.accept(this, ()))}):${postfix(node)}
     """
  }

  override def visit(node: Expression.Pack, a: Unit): String = {
    s"(pack ${node.value.accept(this, ())}):${postfix(node)}"
  }

  override def visit(node: Expression.Identifier, a: Unit): String = {
    s"${node.value}:${postfix(node)}${view(node)}"
  }

  override def visit[C](node: Expression.Const[C], a: Unit): String = {
    s"${node.value}:${postfix(node)}"
  }

  override def visit(node: Expression.Size, a: Unit): String = {
    s"${node.value}:${postfix(node)}"
  }
}

object AstPrinter {
  def print(node: Lift) = {
    (new AstPrinter).visit(node, ())
  }
}
