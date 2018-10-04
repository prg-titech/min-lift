package pass

import ast._
import pass._

class CodeGenerator extends Visitor[Unit, String] {
  val defaultArg = ()

  val chunkSize = 5

  var tempCount = 0
  def mkTemp: String = {
    tempCount += 1
    interVarName
  }
  def interVarName: String = {
    s"temp${tempCount}"
  }

  def visit(node: Lift, arg: ArgumentType): ResultType = {
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
      |
      |     // TODO: remove unnecessary copy
      |     for (int i = 0; i < N; i++) {
      |         result[i] = $interVarName[i];
      |     }
      |}
    """.stripMargin
  }

  override def visit(node: Expression.Apply, arg: ArgumentType): ResultType = {
    node.callee match {
      case Expression.Identifier("mapSeq", false) => {
        val Expression.Lambda(args, body) = node.args(0)

        var prevCode = ""

        val collectionName = node.args(1) match {
          case Expression.Identifier(name, false) => name
          case arg@_ => {
            prevCode = arg.accept(this, ())
            interVarName
          }
        }

        val Type.Array(inner, length) = node.args(1).ty.get

        val result = mkTemp

        s"""
           |$prevCode
           |// local ${inner.toCL} $result[${length.toCL}];
           |${node.addressSpace.getOrElse(MemoryAllocator.PrivateMemory).toCL} ${inner.toCL} $result[64];
           |{
           |  for (int i = 0; i < ${length.toCL}; i++) {
           |    ${inner.toCL} ${args(0).value} = ${collectionName}[i];
           |    $result[i] = ${node.args(0).accept(this, ())};
           |  }
           |}
         """.stripMargin
      }
      case Expression.Identifier("reduceSeq", false) => {
        val init = node.args(0).accept(this, ())
        val Expression.Lambda(args, body) = node.args(1)
        val Expression.Identifier(collectionName, _) = node.args(2)
        val Type.Array(inner, length) = node.args(2).ty.get
        val resultType = node.callee.ty.get.asInstanceOf[Type.Arrow].lastResultType

        s"""
           |{
           |  ${resultType.toCL} ${args(0).value} = ${init}
           |  for (int i = 0; i < ${length.toCL}; i++) {
           |    ${inner.toCL} ${args(1).value} = ${collectionName}[i];
           |    ${args(0).value} = ${node.args(1).accept(this, ())};
           |  }
           |}
         """.stripMargin
      }
      case Expression.Identifier("join", false) => {
        // do nothing
        ""
      }
      case Expression.Identifier("toLocal", false) => node.args(0).accept(this, ())
      case Expression.Identifier("toPrivate", false) => node.args(0).accept(this, ())
      case Expression.Identifier("*", false) => {
        s"""
           |${node.args(0).accept(this, ())} * ${node.args(1).accept(this, ())}
         """.stripMargin
      }
      case Expression.Identifier("+", false) => {
        s"""
           |${node.args(0).accept(this, ())} + ${node.args(1).accept(this, ())}
         """.stripMargin
      }
    }
  }

  override def visit(node: Expression.Lambda, arg: ArgumentType): ResultType = {
    node.body.accept(this, ())
  }

  override def visit(node: Expression.Identifier, arg: ArgumentType): ResultType = node.value

  override def visit[U](node: Expression.Const[U], arg: ArgumentType): ResultType = node.value.toString

  override def visit(node: Expression.Size, arg: ArgumentType): ResultType = node.value.toString
}

object CodeGenerator {
  def generate(node: Lift) = (new CodeGenerator).visit(node, ())
}
