package pass

import scala.collection._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import ast._
import pass.MemoryAllocator._

class CodeGenerator extends ExpressionVisitor[Unit, String] {
  val defaultArg = ()

  val chunkSize = 5

  /*
  val passingVarStack = new mutable.MutableList[CLVariable]()

  var tempCount = 0
  def mkTemp: Unit = {
    tempCount += 1
    passingVarStack += CLVariable(s"temp$tempCount")
  }
  def currentVar = passingVarStack.last
  */

  var indexVarCount = 0
  def mkIndexVar = {
    indexVarCount += 1
    s"i$indexVarCount"
  }

  def safeAcceptAndPop(value: Option[Expression]) = {
    val code = value.map(_.accept(this, ()))
    (varStack.pop(), code.getOrElse(""))
  }

  val varStack = new mutable.ArrayStack[CodeVariable]
  def currentVar = varStack.top

  var tempCount = 0
  def generateResult(addressSpace: Option[AddressSpace], ty: Type) = {
    if (addressSpace == Some(GlobalMemory)) {
      varStack.push(CodeVariable("result", ty))
      ("result", "")
    } else {
      tempCount += 1
      varStack.push(CodeVariable(s"temp$tempCount", ty))
      val r = currentVar.code
      val arrayPostfix = if (ty.isInstanceOf[Type.Scalar]) { "" } else { "[64]" }
      (r, s"${addressSpace.getOrElse(MemoryAllocator.PrivateMemory).toCL} ${dereferenceTypeStr(ty.toCL)} $r$arrayPostfix;")
    }
  }

  def dereferenceTypeStr(typeStr: String) = {
    if (typeStr.endsWith("*")) {
      typeStr.slice(0, typeStr.length - 1)
    }
    else {
      typeStr
    }
  }

  def visit(node: Lift, arg: ArgumentType): ResultType = {
    val Expression.Lambda(params, body) = node.body

    def generateParam(ty: Type, param: Expression.Identifier): String = {
      s"const global ${ty.toCL} restrict ${param.value}"
    }

    // The result type is just a pointer.
    val resultType = body.ty.toCL.takeWhile(_ != '*') + "*";

    // FIXME: can use multiple input
    varStack.push(CodeVariable(params(0).value, params(0).ty))

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

  override def visit(node: Expression.Apply, arg: ArgumentType): ResultType = {
    node.callee match {
      case Expression.Identifier(id, false) => {
        id match {
          case "mapSeq" | "mapGlb" => {
            val func = node.args(0)
            val (collection, prevCode) = safeAcceptAndPop(node.args.lift(1))

            val Type.Array(inner, length) = node.callee.ty.asInstanceOf[Type.Arrow].nthArg(1) // collection.ty

            val resultType = node.ty
            val (result, resultDecl) = generateResult(node.addressSpace, resultType)

            val vi = if (id == "mapSeq") { mkIndexVar } else { "gid" }

            varStack.push(CodeVariable(s"${collection.code}[$vi]", inner))
            val funcCode = func.accept(this, ())
            val resultCode = varStack.pop().code

            varStack.push(CodeVariable(result, resultType))

            id match {
              case "mapSeq" => {
                s"""
                   |// view = ${node.view}, code = ${ViewConstructor.construct(node.view)}
                   |$prevCode
                   |$resultDecl
                   |{
                   |  for (int $vi = 0; $vi < ${length.toCL}; $vi++) {
                   |    $funcCode
                   |    $result[$vi] = $resultCode;
                   |  }
                   |}
                """.stripMargin
              }
              case "mapGlb" => {
                s"""
                   |$prevCode
                   |$resultDecl
                   |{
                   |  int $vi = get_global_id(0);
                   |  $funcCode
                   |  $result[$vi] = $resultCode;
                   |}
                 """.stripMargin
              }
            }
          }
          case "reduceSeq" => {
            val init = node.args(0)
            val func = node.args(1)
            var (collection, prevCode) = safeAcceptAndPop(node.args.lift(2))

            prevCode += init.accept(this, ()) + "\n"
            val initCode = varStack.pop()

            val acc = CodeVariable("acc", init.ty)

            varStack.push(acc)
            varStack.push(CodeVariable(ViewConstructor.construct(node.view).right.get, init.ty))
            val funcCode = func.accept(this, ())
            val resultCode = varStack.pop().code

            val Type.Array(inner, length) = node.callee.ty.asInstanceOf[Type.Arrow].nthArg(2)

            val vi = mkIndexVar

            varStack.push(acc)

            s"""
               |$prevCode
               |${init.ty.toCL} ${acc.code} = $initCode;
               |{
               |  for (int $vi = 0; $vi < ${length.toCL}; $vi++) {
               |    $funcCode
               |    ${acc.code} = $resultCode;
               |  }
               |}
            """.stripMargin
          }
          case "join" => {
            // do nothing
            ""
          }
          case "split" => {
            // do nothing
            ""
          }
          case "toGlobal" => node.args(0).accept(this, ())
          case "toLocal" => node.args(0).accept(this, ())
          case "toPrivate" => node.args(0).accept(this, ())
          // case id@(Expression.Identifier("+", false) | Expression.Identifier("*", false)) => {
          case op@("+" | "*") => {
            val resultType = node.callee.ty.representativeType

            val (left, prevLeft) = safeAcceptAndPop(node.args.lift(0))
            val (right, prevRight) = safeAcceptAndPop(node.args.lift(1))

            val (result, resultDecl) = generateResult(None, resultType)

            s"""
               |$resultDecl
               |$prevLeft
               |$prevRight
               |$result = ${left.code} $op ${right.code};
            """.stripMargin
          }
        }
      }
    }
  }

  override def visit(node: Expression.Lambda, arg: ArgumentType): ResultType = {
    val argDecls = node.args.reverse.map(arg => {
      val argCode = varStack.pop()
      s"${arg.ty.toCL} ${arg.value} = $argCode;"
    }).mkString("\n")

    node.args.foreach(arg => {
      varStack.push(CodeVariable(arg.value, arg.ty))
    })

    val bodyCode = node.body.accept(this, ())
    val resultCode = varStack.pop()

    val resultType = node.ty.representativeType
    val (result, resultDecl) = generateResult(None, resultType)

    varStack.push(CodeVariable(result, resultType))

    s"""
       |$argDecls
       |$resultDecl
       |$bodyCode
       |$result = $resultCode;
     """.stripMargin
  }

  override def visit(node: Expression.Identifier, arg: ArgumentType): ResultType = ""

  override def visit[U](node: Expression.Const[U], arg: ArgumentType): ResultType = {
    val (result, resultDecl) = generateResult(None, node.ty)
    varStack.push(CodeVariable(result, node.ty))
    s"""
       |$resultDecl
       |$result = ${node.toCL};
     """.stripMargin
  }

  override def visit(node: Expression.Size, arg: ArgumentType): ResultType = node.value.toString
}

object CodeGenerator {
  def generate(node: Lift) = (new CodeGenerator).visit(node, ())
}

case class CLVariable(name: String) {
  override def toString: String = name
}

case class CodeVariable(code: String, typ: Type) {
  override def toString: String = code
}
