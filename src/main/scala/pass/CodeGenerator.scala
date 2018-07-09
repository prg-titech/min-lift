package pass

import ast._

class CodeGenerator extends Visitor[String] {
  override def visit(node: Lift): String = {
    val Expression.Lambda(params, body) = node.body

    def generateParam(ty: Type, param: Expression.Identifier): String = {
      s"const global ${ty.toCL} restrict ${param.value}"
    }

    s"""
      |kernel void KERNEL(
      |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
      |  global ${body.ty.toCL} result,
      |  ${node.variables.map(v => s"int ${v.name}").mkString(", ")}) {
      |     ${body.accept(this)}
      |  }
    """.stripMargin
  }

  override def visit[U](node: Expression.Apply[U]): String = ???

  override def visit(node: Expression.Lambda) = ???

  override def visit(node: Expression.Map) = ???

  override def visit(node: Expression.Identifier) = node.value

  override def visit[U](node: Expression.Const[U]) = ???

  override def visit(node: Expression.Undefined) = ???
}
