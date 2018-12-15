package pass

import scala.collection._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import ast._
import pass.MemoryAllocator._

class CodeGenerator extends ExpressionVisitor[Unit, String] {
  val defaultArg = ()

  val chunkSize = 5

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
  def generateResult(addressSpace: Option[AddressSpace], ty: Type, dynamic: Boolean = false) = {
    val global = addressSpace == Some(GlobalMemory)
    val name = if (global) {
      "result"
    } else {
      tempCount += 1
      s"temp$tempCount"
    }

    val vc = if (dynamic) {
      CodeDynArrayVariable(name, CodeVariable(s"ary_size"))
    } else {
      CodeVariable(name)
    }

    varStack.push(vc)

    if (global) {
      (currentVar, "")
    } else {
      val arrayPostfix = if (ty.isInstanceOf[Type.Scalar]) { "" } else { "[64]" }
      (currentVar, s"${addressSpace.getOrElse(MemoryAllocator.PrivateMemory).toCL} ${dereferenceTypeStr(ty.toCL)} ${currentVar.code}$arrayPostfix;")
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
    varStack.push(CodeVariable(params(0).value))

    val config = (
      "ChunkSize" -> chunkSize
    )

    val bodyCode = body.accept(this, ())

    val postfixCode = varStack.top match {
      case CodeVariable("result") => ""
      case CodeVariable(name) => {
        body.ty match {
          case Type.Array(_, Type.SizeConst(1)) => {
            s"result[0] = $name;"
          }
          case _ => ""
        }
      }
    }

    s"""// ${compact(render(config))}
      |
      |kernel void KERNEL(
      |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
      |  global $resultType result,
      |  ${node.variables.map(v => s"int ${v.toCL}").mkString(", ")}) {
      |
      |     int ary_size = 0;
      |
      |     $bodyCode
      |     $postfixCode
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

            val vi = if (id == "mapSeq") { mkIndexVar } else { "gid" }

            varStack.push(CodeVariable(s"${collection.code}[$vi]"))
            val funcCode = func.accept(this, ())
            val resultCode = varStack.pop()

            val (result, resultDecl) = generateResult(node.addressSpace, resultType)

            id match {
              case "mapSeq" => {
                s"""
                   |// view = ${node.view}, code = ${ViewConstructor.construct(node.view)}
                   |$prevCode
                   |$resultDecl
                   |{
                   |  for (int $vi = 0; $vi < ${collection.size(length.toCL)}; $vi++) {
                   |    $funcCode
                   |    ${result.assign(vi, resultCode)}
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
                   |  ${result.assign(vi, resultCode)}
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

            val acc = CodeVariable("acc")

            varStack.push(acc)
            varStack.push(CodeVariable(ViewConstructor.construct(node.view).right.get))
            val funcCode = func.accept(this, ())
            val resultCode = varStack.pop().code

            val Type.Array(inner, length) = node.callee.ty.asInstanceOf[Type.Arrow].nthArg(2)

            val vi = mkIndexVar

            varStack.push(acc) // result

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
          case "filterSeq" => {
            val func = node.args(0)
            var (collection, prevCode) = safeAcceptAndPop(node.args.lift(1))

            val Type.Array(inner, length) = node.callee.ty.asInstanceOf[Type.Arrow].nthArg(1) // collection.ty

            val resultType = node.ty

            val vi = mkIndexVar

            val input = CodeVariable(s"${collection.code}[$vi]")

            varStack.push(input)
            val funcCode = func.accept(this, ())
            val resultCode = varStack.pop().code

            val (result, resultDecl) = generateResult(node.addressSpace, resultType, true)

            s"""
               |$prevCode
               |$resultDecl
               |{
               |  int idx = 0;
               |  for (int $vi = 0; $vi < ${length.toCL}; $vi++) {
               |    $funcCode
               |    if ($resultCode) {
               |      ${result.assign("idx++", input)}
               |    }
               |  }
               |  ${result.assignSize("idx")}
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
          case op@("+" | "*" | "<") => {
            val resultType = node.callee.ty.representativeType

            val (left, prevLeft) = safeAcceptAndPop(node.args.lift(0))
            val (right, prevRight) = safeAcceptAndPop(node.args.lift(1))

            val (result, resultDecl) = generateResult(None, resultType)

            s"""
               |$resultDecl
               |$prevLeft
               |$prevRight
               |${result.code} = ${left.code} $op ${right.code};
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
      varStack.push(CodeVariable(arg.value))
    })

    val bodyCode = node.body.accept(this, ())
    val resultCode = varStack.pop()

    val resultType = node.ty.representativeType
    val (result, resultDecl) = generateResult(None, resultType)

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

class CodeVariable(val code: String) {
  override def toString: String = code

  def assign(idx: String, value: CodeVariable) = s"$code[$idx] = ${value.code};"
  def assignSize(size: String) = ""
  def size(alt: String) = alt
}
object CodeVariable {
  def apply(code: String) = new CodeVariable(code)
  def unapply(arg: CodeVariable): Option[String] = Some(arg.code)
}

case class CodeDynArrayVariable(c: String, sizeVar: CodeVariable) extends CodeVariable(c) {
  override def assignSize(size: String): String = s"${sizeVar.code} = $size;"
  override def size(alt: String): String = sizeVar.toString
}
