package pass

import ast._

class CodeGenerator extends Visitor[Unit, String] {
  type ArgType = Unit

  override def visit(node: Lift, arg: ArgType): String = {
    val Expression.Lambda(params, body) = node.body

    def generateParam(ty: Type, param: Expression.Identifier): String = {
      s"const global ${ty.toCL} restrict ${param.value}"
    }

    s"""
      |kernel void KERNEL(
      |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
      |  global ${body.ty.get.toCL} result,
      |  ${node.variables.map(v => s"int ${v.toCL}").mkString(", ")}) {
      |     int gid = get_global_id(0);
      |     ${body.accept(this, ())}
      |  }
    """.stripMargin
  }

  override def visit(node: Expression.Apply, arg: ArgType): String = {
    node.callee match {
      case Expression.Identifier("mapSeq", false) => {
        val Expression.Lambda(args, body) = node.args(0)
        val Expression.Identifier(collectionName, _) = node.args(1)
        val Type.Array(inner, length) = node.args(1).ty.get
        val chunkSize = 5
        s"""
           |int n = $chunkSize;
           |int diff = $chunkSize * gid - N + 1;
           |if (diff > 0) n = min(n, diff);
           |for (int i = $chunkSize * gid; i < $chunkSize * gid + n; i++) {
           |  ${inner.toCL} ${args(0).value} = ${collectionName}[i];
           |  result[i] = ${node.args(0).accept(this, ())};
           |}
         """.stripMargin
      }
      case Expression.Identifier("*", false) => {
        s"""
           |${node.args(0).accept(this, ())} * ${node.args(1).accept(this, ())}
         """.stripMargin
      }
    }
  }

  override def visit(node: Expression.Lambda, arg: ArgType): String = {
    node.body.accept(this, ())
  }

  override def visit(node: Expression.Identifier, arg: ArgType): String = node.value

  override def visit[U](node: Expression.Const[U], arg: ArgType): String = node.value.toString

  override def visit(node: Expression.Size, arg: ArgType): String = node.value.toString
}

object CodeGenerator {
  def generate(node: Lift) = (new CodeGenerator).visit(node, ())
}
