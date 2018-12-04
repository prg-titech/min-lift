package pass

import scala.collection._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import ast._
import pass.MemoryAllocator._

class CodeGenerator extends ExpressionVisitor[Unit, String] {
  val defaultArg = ()

  val chunkSize = 5

  val passingVarStack = new mutable.MutableList[CLVariable]()

  var tempCount = 0
  def mkTemp: Unit = {
    tempCount += 1
    passingVarStack += CLVariable(s"temp$tempCount")
  }
  def currentVar = passingVarStack.last

  def visit(node: Lift, arg: ArgumentType): ResultType = {
    val Expression.Lambda(params, body) = node.body

    def generateParam(ty: Type, param: Expression.Identifier): String = {
      s"const global ${ty.toCL} restrict ${param.value}"
    }

    // The result type is just a pointer.
    val resultType = body.ty.toCL.takeWhile(_ != '*') + "*";

    // FIXME: can use multiple input
    passingVarStack += CLVariable(params(0).value)

    val config = (
      "ChunkSize" -> chunkSize
    )

    s"""// ${compact(render(config))}
      |
      |kernel void KERNEL(
      |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
      |  global $resultType result,
      |  ${node.variables.map(v => s"int ${v.toCL}").mkString(", ")}) {
      |     ${body.accept(this, ())}
      |}
    """.stripMargin
  }

  def generateResult(addressSpace: Option[AddressSpace], ty: Type) = {
    if (addressSpace == Some(GlobalMemory)) {
      passingVarStack += CLVariable("result")
      ("result", "")
    } else {
      mkTemp
      val r = currentVar.name
      (r, s"${addressSpace.getOrElse(MemoryAllocator.PrivateMemory).toCL} ${ty.toCL} $r[64];")
    }
  }

  override def visit(node: Expression.Apply, arg: ArgumentType): ResultType = {
    node.callee match {
      case Expression.Identifier("mapSeq", false) => {
        val Expression.Lambda(args, _) = node.args(0)

        var prevCode = ""

        val collectionName = node.args(1) match {
          case Expression.Identifier(name, false) => name
          case arg@_ => {
            prevCode = arg.accept(this, ())
            currentVar.name
          }
        }

        val Type.Array(inner, length) = node.args(1).ty

        val (result, resultDecl) = generateResult(node.addressSpace, inner)

        s"""
           |// view = ${node.view}
           |$prevCode
           |$resultDecl
           |{
           |  for (int i = 0; i < ${length.toCL}; i++) {
           |    ${inner.toCL} ${args(0).value} = $collectionName[i];
           |    $result[i] = ${node.args(0).accept(this, ())};
           |  }
           |}
         """.stripMargin
      }
      case Expression.Identifier("mapGlb", false) => {
        val Expression.Lambda(args, _) = node.args(0)

        var prevCode = ""

        val collectionName = node.args(1) match {
          case Expression.Identifier(name, false) => name
          case arg@_ => {
            prevCode = arg.accept(this, ())
            currentVar.name
          }
        }

        val Type.Array(inner, _) = node.args(1).ty

        val (result, resultDecl) = generateResult(node.addressSpace, inner)

        s"""
           |$prevCode
           |$resultDecl
           |{
           |  int gid = get_global_id(0);
           |  ${inner.toCL} ${args(0).value} = $collectionName[gid];
           |  $result[gid] = ${node.args(0).accept(this, ())};
           |}
         """.stripMargin
      }
      case Expression.Identifier("reduceSeq", false) => {
        val init = node.args(0).accept(this, ())
        val Expression.Lambda(args, body) = node.args(1)
        val Expression.Identifier(collectionName, _) = node.args(2)
        val Type.Array(inner, length) = node.args(2).ty
        val resultType = node.callee.ty.asInstanceOf[Type.Arrow].lastResultType

        val (result, resultDecl) = generateResult(node.addressSpace, inner)

        val acc = args(0).value

        s"""
           |// view = ${node.view}
           |$resultDecl
           |{
           |  ${resultType.toCL.stripSuffix("*")} $acc = $init;
           |  for (int i = 0; i < ${length.toCL}; i++) {
           |    ${inner.toCL} ${args(1).value} = ${collectionName}[i];
           |    $acc = ${node.args(1).accept(this, ())};
           |  }
           |  $result[0] = $acc;
           |
           |}
         """.stripMargin
      }
      case Expression.Identifier("join", false) => {
        // do nothing
        ""
      }
      case Expression.Identifier("split", false) => {
        // do nothing
        ""
      }
      case Expression.Identifier("toGlobal", false) => node.args(0).accept(this, ())
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

  override def visit[U](node: Expression.Const[U], arg: ArgumentType): ResultType = node.toCL

  override def visit(node: Expression.Size, arg: ArgumentType): ResultType = node.value.toString
}

object CodeGenerator {
  def generate(node: Lift) = (new CodeGenerator).visit(node, ())
}

case class CLVariable(name: String)