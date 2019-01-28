package pass

import scala.collection._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import ast._
import pass.MemoryAllocator._
import structures._

class CodeGenerator extends ExpressionVisitor[Environment[CodeVariable], String] {
  val chunkSize = 5

  var indexVarCount = 0

  val varStack = new mutable.ArrayStack[CodeVariable]
  def currentVar = varStack.top

  var tempCount = 0

  var splitKernel = false

  val prefixSumKernel =
    """
      |kernel void prefix_sum(global int *xs, global int *ys, int i) {
      |  global int *in = NULL, *out = NULL;
      |  if (i % 2 == 0) {
      |    in = xs; out = ys;
      |  }
      |  else {
      |    in = ys; out = xs;
      |  }
      |
      |  int j = get_global_id(0);
      |
      |  int powiof2 = pown(2.0f, i);
      |  if (j < powiof2) {
      |    out[j] = in[j];
      |  }
      |  else {
      |    out[j] = in[j] + in[j - powiof2];
      |  }
      |}
    """.stripMargin

  def mkIndexVar = {
    indexVarCount += 1
    s"i$indexVarCount"
  }

  def safeAcceptAndPop(value: Option[Expression]) = {
    val code = value.map(_.accept(this, EmptyEnvironment[CodeVariable]())) // FIXME
    (varStack.pop(), code.getOrElse(""))
  }

  def generateResult(addressSpace: Option[AddressSpace], ty: Type, dynamic: Boolean = false) = {
    val global = addressSpace == Some(GlobalMemory)
    val name = if (global) {
      "result"
    } else {
      tempCount += 1
      s"temp$tempCount"
    }

    val vc = if (dynamic) {
      val Type.Existential(Type.Array(_, size)) = ty
      val sizeStr = size match {
        case Type.TypeVar(_) => "ary_size"
        case _ => size.toCL
      }
      CodeDynArrayVariable(name, CodeVariable(sizeStr))
    } else {
      CodeVariable(name)
    }

    varStack.push(vc)

    if (global) {
      (currentVar, "")
    } else {
      val mod   = addressSpace.getOrElse(MemoryAllocator.PrivateMemory).toCL
      val id    = currentVar.code
      val tyStr = dereferenceTypeStr(ty.toCL)
      val arrayPostfix = ty match {
        case Type.Scalar(_)/* | Type.Array(_, Type.SizeDynamicInstance(_))*/ => ""
        case _ => "[64]"
      }
      (currentVar, s"$mod $tyStr $id$arrayPostfix;")
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

  def varDecl(name: Expression.Identifier, ty: Type, addrSpace: Option[AddressSpace], code: CodeVariable): String = {
    if (ty.isInstanceOf[Type.Tuple2]) {
      ""
    }
    else {
      s"${addrSpace.map(_.toCL).getOrElse("")} ${ty.toCL} ${name.value} = $code;"
    }
  }

  def visit(node: Lift, env: ArgumentType): ResultType = {
    val Expression.Lambda(params, body) = node.body

    def generateParam(ty: Type, param: Expression.Identifier): String = {
      s"const global ${ty.toCL} restrict ${param.value}"
    }

    // The result type is just a pointer.
    val resultType = body.ty.toCL.takeWhile(_ != '*') + "*";

    params.foreach(param => varStack.push(CodeVariable(param.value)))

    val bodyCode = body.accept(this, env)

    val postfixCode = (varStack.top match {
      case CodeVariable("result") => ""
      case CodeVariable(name) => {
        body.ty match {
          case Type.Array(_, Type.SizeConst(1)) => {
            s"result[0] = $name;"
          }
          case _ => ""
        }
      }
    }) + "\n" + (body.ty match {
      case Type.Existential(Type.Array(_, _)) => {
        "*result_size = ary_size;"
      }
      case Type.Array(_, size) => {
        s"*result_size = ${size.toCL};"
      }
      case _ => {
        "*result_size = 1;"
      }
    })

    val config =
        ("ChunkSize" -> chunkSize) ~
        ("InputSize" -> node.inputTypes.size) ~
        ("UseFilterGlobal" -> splitKernel)

    if (splitKernel) {
      val codeChunks = bodyCode.split("// ---", 2)

      s"""// ${compact(render(config))}
        |
        |kernel void KERNEL(
        |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
        |  global $resultType result,
        |  global int* result_size,
        |  global int* bitmap,
        |  ${node.variables.map(v => s"int ${v.toCL}").mkString(", ")}) {
        |     // int ary_size = 0;
        |
        |     ${codeChunks(0)}
        |}
        |
        |kernel void KERNEL2(
        |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
        |  global $resultType result,
        |  global int* result_size,
        |  global int* bitmap,
        |  global int* indices,
        |  ${node.variables.map(v => s"int ${v.toCL}").mkString(", ")}) {
        |     int ary_size = 0;
        |     ${codeChunks(1)}
        |     $postfixCode
        |}
        |
        |$prefixSumKernel
      """.stripMargin
    }
    else {
      s"""// ${compact(render(config))}
        |
        |kernel void KERNEL(
        |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
        |  global $resultType result,
        |  global int* result_size,
        |  ${node.variables.map(v => s"int ${v.toCL}").mkString(", ")}) {
        |
        |     // int ary_size = 0;
        |
        |     $bodyCode
        |     $postfixCode
        |}
      """.stripMargin
    }
  }

  override def visit(node: Expression.Apply, env: ArgumentType): ResultType = {
    node.callee match {
      case Expression.Identifier(id, false) => {
        id match {
          case "mapSeq" | "mapGlb" | "mapWrg" | "mapLcl" => {
            val func = node.args(0)
            val (collection, prevCode) = safeAcceptAndPop(node.args.lift(1))

            val Type.Array(inner, length) = node.callee.ty.asInstanceOf[Type.Arrow].nthArg(1) // collection.ty

            val resultType = node.ty

            val vi = mkIndexVar

            val index = ViewConstructor.construct(node.view).getOrElse(s"${collection.code}[$vi]")
            varStack.push(CodeVariable(index))
            val funcCode = func.accept(this, env)
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
              case "mapWrg" => {
                s"""
                   |// view = ${node.view}, code = ${ViewConstructor.construct(node.view)}
                   |$prevCode
                   |$resultDecl
                   |{
                   |  int $vi = get_group_id(0);
                   |  $funcCode
                   |  // ${result.assign(vi, resultCode)}
                   |}
                 """.stripMargin
              }
              case "mapLcl" => {
                s"""
                   |// view = ${node.view}, code = ${ViewConstructor.construct(node.view)}
                   |$prevCode
                   |$resultDecl
                   |{
                   |  int $vi = get_local_id(0);
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

            prevCode += init.accept(this, env) + "\n"
            val initCode = varStack.pop()

            val acc = CodeVariable("acc")

            varStack.push(acc)
            varStack.push(CodeVariable(ViewConstructor.construct(node.view).right.get))
            val funcCode = func.accept(this, env)
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
            val funcCode = func.accept(this, env)
            val resultCode = varStack.pop().code

            val (result, resultDecl) = generateResult(node.addressSpace, resultType, true)

            s"""
               |$prevCode
               |$resultDecl
               |int ${result.size("len")} = 0;
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
          case "filterGlb" => {

            splitKernel = true

            val func = node.args(0)
            var (collection, prevCode) = safeAcceptAndPop(node.args.lift(1))

            val Type.Array(inner, length) = node.callee.ty.asInstanceOf[Type.Arrow].nthArg(1) // collection.ty

            val vi = mkIndexVar

            val input = CodeVariable(s"${collection.code}[$vi]")

            varStack.push(input)
            val funcCode = func.accept(this, env)
            val resultCode = varStack.pop().code

            val resultType = node.ty

            val (result, resultDecl) = generateResult(node.addressSpace, resultType, true)

            s"""
               |int $vi = get_global_id(0);
               |$funcCode
               |bitmap[$vi] = $resultCode;
               |
               |// ---
               |
               |int $vi = get_global_id(0);
               |if (bitmap[$vi]) {
               |  ${result.assign(s"indices[$vi] - 1", input)}
               |}
               |${result.assignSize(s"indices[${length.toCL} - 1]")}
             """.stripMargin
          }
          case "join" => {
            node.args(0).accept(this, env)
          }
          case "split" | "zip" => {
            // do nothing
            ""
          }
          case "get1" | "get2" => {
            varStack.push(CodeVariable(ViewConstructor.construct(node.view).right.get))
            ""
          }
          case "toGlobal" | "toLocal" | "toPrivate" => node.args(0).accept(this, env)
          case op@("+" | "*" | "<" | ">") => {
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

  override def visit(node: Expression.Lambda, env: ArgumentType): ResultType = {
    val argDecls = node.args.reverse.map(arg => {
      val argCode = varStack.pop()
      varDecl(arg, arg.ty, None, argCode)
    }).mkString("\n")

    val bodyCode = node.body.accept(this, env)

    s"""
       |$argDecls
       |$bodyCode
     """.stripMargin
  }

  override def visit(node: Expression.Let, env: ArgumentType): ResultType = {
    val valueCode = node.value.accept(this, env)
    val valueDecl = varDecl(node.id, node.value.ty, node.value.addressSpace, varStack.pop())

    val bodyCode = node.body.accept(this, env)

    s"""
       |$valueCode
       |$valueDecl
       |$bodyCode
     """.stripMargin
  }

  override def visit(node: Expression.Identifier, arg: ArgumentType): ResultType = {
    varStack.push(CodeVariable(node.value))
    ""
  }

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
  def generate(node: Lift) = (new CodeGenerator).visit(node, EmptyEnvironment[CodeVariable]())
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
