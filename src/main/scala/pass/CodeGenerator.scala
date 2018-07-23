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
      |     int gid = get_global_id(0);
      |     printf("thread # = %d\\n", gid);
      |     ${body.accept(this)}
      |  }
    """.stripMargin
  }

  override def visit[U](node: Expression.Apply): String = {
    node.callee match {
      case Expression.Identifier("mapSeq", false) => {
        val Expression.Lambda(args, body) = node.args(0)
        val Expression.Identifier(collectionName, _) = node.args(1)
        val Type.Array(inner, length) = node.args(1).ty
        val chunkSize = 5
        s"""
           |int n = $chunkSize;
           |int diff = $chunkSize * gid - N + 1;
           |if (diff > 0) n = min(n, diff);
           |for (int i = $chunkSize * gid; i < $chunkSize * gid + n; i++) {
           |  ${inner.toCL} ${args(0).value} = ${collectionName}[i];
           |  result[i] = ${node.args(0).accept(this)};
           |}
         """.stripMargin
      }
      case Expression.Identifier("*", false) => {
        s"""
           |${node.args(0).accept(this)} * ${node.args(1).accept(this)}
         """.stripMargin
      }
    }
  }

  override def visit(node: Expression.Lambda): String = {
    node.body.accept(this)
  }

  override def visit(node: Expression.Map) = ???

  override def visit(node: Expression.Identifier): String = node.value

  override def visit[U](node: Expression.Const[U]): String = node.value.toString

  override def visit(node: Expression.Undefined) = ???
}

object CodeGenerator {
  def generate(node: Lift) = (new CodeGenerator).visit(node)
}
