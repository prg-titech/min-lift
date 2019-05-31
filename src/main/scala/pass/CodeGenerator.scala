package pass

import scala.collection._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import ast._
import lib._

class CodeGenerator extends ExpressionVisitor[Environment[Code], Code] {

  val chunkSize = 5
  val funcs = lift.BuiltInFunctions.getFuncs(new UniqueIdGenerator())

  var splitKernel = false

  val indexVarGen = new UniqueIdGenerator("i")
  val interimResultVarGen = new UniqueIdGenerator()
  val tempVarGen  = new UniqueIdGenerator("v")

  val addressSpaceStack = new mutable.ArrayStack[AddressSpace]()

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

  def generateResult(ty: Type, useAddressSpace: Boolean, dynamic: Boolean = false) = {
    val addressSpace = if (useAddressSpace) {
      addressSpaceStack.lift(0)
    } else {
      Some(PrivateMemory)
    }

    val global = addressSpace == Some(GlobalMemory)
    val name = if (global) {
      "result"
    } else {
      tempVarGen.generateString()
    }

    val resultVar = if (dynamic) {
      val Type.Existential(Type.Array(_, size)) = ty
      val sizeStr = size match {
        case Type.TypeVar(_) => "ary_size"
        case _ => size.toCL
      }
      DynArrayVariable(name, Variable(sizeStr))
    } else {
      Variable(name)
    }

    if (global) {
      (resultVar, "")
    } else {
      val mod   = addressSpace.getOrElse(PrivateMemory).toCL
      val id    = resultVar.code
      val tyStr = dereferenceTypeStr(ty.toCL)
      val arrayPostfix = ty match {
        case Type.Scalar(_)/* | Type.Array(_, Type.SizeDynamicInstance(_))*/ => ""
        case Type.Array(_, Type.SizeConst(1)) => ""
        case _ => "[64]"
      }
      (resultVar, s"$mod $tyStr $id$arrayPostfix;")
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

  def varDecl(name: Expression.Identifier, ty: Type, addrSpace: Option[AddressSpace], code: Variable): String = {
    if (ty.isInstanceOf[Type.Tuple2]) {
      ""
    }
    else {
      s"${addrSpace.map(_.toCL).getOrElse("")} ${ty.toCL} ${name.value} = $code;"
    }
  }

  def visit(node: Lift, env: ArgumentType): String = {

    def generateParam(ty: Type, param: Expression.Identifier): String = {
      s"const global ${ty.toCL} restrict ${param.value}"
    }

    val Expression.Lambda(params, body) = node.body

    // The result type is just a pointer.
    val resultType = body.ty.toCL.takeWhile(_ != '*') + "*";

    val args = params.map(param => GeneratedCode("", Variable(param.value), param.ty))
    val res = generateApplyLambda(node.body, args, env)
    val GeneratedCode(bodyCode, result, _) = res

    val postfixCode = (result match {
      case Variable("result") => ""
      case Variable(name) => {
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
        ("KernelCount" -> (if (splitKernel) 3 else 1))

    if (splitKernel) {
      val codeChunks = bodyCode.split("// ---", 3)

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
        |kernel void KERNEL3(
        |  ${node.inputTypes.zip(params).map { case (ty, param) => generateParam(ty, param) }.mkString(",\n")},
        |  global $resultType result,
        |  global int* result_size,
        |  ${node.variables.map(v => s"int ${v.toCL}").mkString(", ")}) {
        |     int ary_size = 0;
        |     ${codeChunks(2)}
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

  def visit(node: Expression.Apply, env: ArgumentType): ResultType = {
    val callee = node.callee.accept(this, env)
    generateApply(callee, node.args, env)
  }

  def generateApply(callee: Code, aargs: List[Expression], env: ArgumentType): ResultType = {
    def g(expr: Expression) = expr.accept(this, env)
    def asGeneratedCode(code: Code) = {
      code match {
        case ExpressionCode(id@Expression.Identifier(value, _)) => {
          GeneratedCode("", Variable(value), id.ty)
        }
        case _ => code
      }
    }

    callee match {
      case ExpressionCode(callee) => {
        generateApply(PartialApplyCode(callee, aargs), List(), env)
      }
      // case PartialApplyCode(callee, pargs, ty@Type.Arrow(_, _)) => {
      case PartialApplyCode(callee, pargs) => {
        val args = pargs ::: aargs

        val calleeType = callee.ty.asInstanceOf[Type.Arrow]

        callee match {
          case Expression.Identifier(name, false) => {
            funcs.lookup(name) match {
              case Some(func) => {
                val arw@Type.Arrow(_, _) = func.ty
                val argCount = arw.args.size

                if (argCount != args.size) {
                  return PartialApplyCode(callee, args)
                }
              }
              case _ => {}
            }

            name match {
              case "mapSeq" | "mapGlb" | "mapWrg" | "mapLcl" => {
                val GeneratedCode(prevCode, collection, ty) = g(args(1))
                val Type.Array(inner, length) = ty

                val vi = indexVarGen.generateString()

                val funcType = calleeType.nthArg(0).asInstanceOf[Type.Arrow]
                val elemExpr = Expression.Identifier(vi, false)
                elemExpr.ty = funcType.nthArg(0)
                val GeneratedCode(funcCode, funcResult, elemTy) = generateApply(
                    g(args(0)), List(elemExpr), env)

                val resultType = calleeType.lastResultType
                val (result, resultDecl) = generateResult(resultType, true)

                val code = name match {
                  case "mapSeq" => {
                    s"""
                       |$prevCode
                       |$resultDecl
                       |{
                       |  for (int $vi = 0; $vi < ${collection.size(length.toCL)}; $vi++) {
                       |    $funcCode
                       |    ${result.assign(vi, funcResult)}
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
                       |  ${result.assign(vi, funcResult)}
                       |}
                 """.stripMargin
                  }
                  case "mapWrg" => {
                    s"""
                       |$prevCode
                       |$resultDecl
                       |{
                       |  int $vi = get_group_id(0);
                       |  $funcCode
                       |  ${result.assign(vi, funcResult)}
                       |}
                 """.stripMargin
                  }
                  case "mapLcl" => {
                    s"""
                       |$prevCode
                       |$resultDecl
                       |{
                       |  int $vi = get_local_id(0);
                       |  $funcCode
                       |  ${result.assign(vi, funcResult)}
                       |}
                 """.stripMargin
                  }
                }
                GeneratedCode(code, result, resultType)
              }
              case "reduceSeq" => {
                val GeneratedCode(initCode, initResult, _) = asGeneratedCode(g(args(0)))
                val GeneratedCode(prevCode, collection, ty) = asGeneratedCode(g(args(2)))
                val Type.Array(inner, length) = ty

                val vi = indexVarGen.generateString()
                val acc = indexVarGen.generateString()

                val funcType = calleeType.nthArg(1).asInstanceOf[Type.Arrow]
                val accExpr  = Expression.Identifier(acc, false)
                accExpr.ty   = funcType.nthArg(0)
                val elemExpr = Expression.Identifier(vi, false)
                elemExpr.ty  = funcType.nthArg(1)
                val GeneratedCode(funcCode, funcResult, elemTy) = generateApply(
                    g(args(1)), List(accExpr, elemExpr), env)

                val resultType = calleeType.lastResultType
                val (result, resultDecl) = generateResult(resultType, true)

                val code = s"""
                   |$prevCode
                   |$resultDecl
                   |$initCode
                   |${accExpr.ty.toCL} $acc = $initResult;
                   |{
                   |  for (int $vi = 0; $vi < ${length.toCL}; $vi++) {
                   |    $funcCode
                   |    $acc = $funcResult;
                   |  }
                   |  $result = $acc;
                   |}
                """.stripMargin

                GeneratedCode(code, result, resultType)
              }
              /*
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
                 |
             |// ---
           """.stripMargin
            }
               */
              case "join" => {
                g(args(0))
              }
              case "split" => {
                g(args(1))
              }
              case "zip" => {
                g(args(0))
              }
              case "get1" | "get2" => {
                val node = args(0)
                val code = ViewConstructor.construct(node.view).right.get
                GeneratedCode("", Variable(code), node.ty)
              }
              case "toGlobal" | "toLocal" | "toPrivate" => {
                val addressSpace = name match {
                  case "toGlobal"  => GlobalMemory
                  case "toLocal"   => LocalMemory
                  case "toPrivate" => PrivateMemory
                }

                addressSpaceStack.push(addressSpace)

                val code = g(args(0))

                addressSpaceStack.pop()

                code match {
                  case p@PartialApplyCode(_, _) => AddressSpaceCode(p, addressSpace)
                  case _ => code
                }
              }
              case op@("+" | "*" | "<" | ">") => {
                val resultType = calleeType.lastResultType

                val GeneratedCode(prevLeft, left, _)   = asGeneratedCode(g(args(0)))
                val GeneratedCode(prevRight, right, _) = asGeneratedCode(g(args(1)))

                val (result, resultDecl) = generateResult(resultType, false)

                val code = s"""
                   |$resultDecl
                   |$prevLeft
                   |$prevRight
                   |${result.code} = ${left.code} $op ${right.code};
                """.stripMargin

                GeneratedCode(code, result, resultType)

              }
            }
          }
          case lambda@Expression.Lambda(largs, body) => {
            if (largs.size == args.size) {
              generateApplyLambda(lambda, args.map(g(_)), env)
            }
            else {
              // PartialApplyCode(lambda, args, ty.reduce(args.size - 1))
              PartialApplyCode(lambda, args)
            }
          }
        }
      }
      case AddressSpaceCode(code, addressSpace) => {
        addressSpaceStack.push(addressSpace)

        val res = generateApply(code, aargs, env)

        addressSpaceStack.pop()

        res match {
          case p@PartialApplyCode(_, _) => AddressSpaceCode(p, addressSpace)
          case _ => res
        }
      }
    }
  }

  def generateApplyLambda(lambda: Expression.Lambda, args: List[Code], env: ArgumentType): ResultType = {
    val env2 = lambda.args.zip(args).foldRight(env.pushEnv(scala.Predef.Map[String, Code]())){ case ((id, code), env) => {
      env.append(id.value, code)
    }}

    lambda.body.accept(this, env2)
  }

  def visit(node: Expression.Lambda, env: ArgumentType): ResultType = {
    PartialApplyCode(node, List())
  }

  def visit(node: Expression.Let, env: ArgumentType): ResultType = {
    val generatedValue = node.value.accept(this, env)
    val env2 = env.pushEnv(Map(node.id.value -> generatedValue))
    node.body.accept(this, env2)

    /*
    val GeneratedCode(valueCode, value, _) = generatedValue
    val valueDecl = varDecl(node.id, node.value.ty, node.value.addressSpace, value)

    val GeneratedCode(bodyCode, result, _) = node.body.accept(this, env2)

    val code = s"""
       |$valueCode
       |$valueDecl
       |$bodyCode
     """.stripMargin
    GeneratedCode(code, result, node.ty)
     */
  }

  def visit(node: Expression.Pack, env: ArgumentType): ResultType = {
    node.value.accept(this, env)
  }

  def visit(node: Expression.Identifier, env: ArgumentType): ResultType = {
    env.lookup(node.value)
      .map(value => {
        ViewConstructor.construct(node.view) match {
          case Right(code) => GeneratedCode("", Variable(code), node.ty)
          case Left(err) => value
        }
      })
      // .map(_ => GeneratedCode("", Variable(node.value), node.ty))
      // .getOrElse(GeneratedCode("", Variable(node.value), node.ty))
      .getOrElse(ExpressionCode(node))
  }

  def visit[C](node: Expression.Const[C], arg: ArgumentType): ResultType = {
    GeneratedCode("", Variable(node.toCL), node.ty)
  }

  def visit(node: Expression.Size, arg: ArgumentType): ResultType = {
    ExpressionCode(node)
  }
}

object CodeGenerator {
  def generate(node: Lift) = (new CodeGenerator).visit(node, EmptyEnvironment[Code]())
}

sealed abstract class AddressSpace(clModifier: String) {
    def toCL: String = clModifier
}
case object PrivateMemory extends AddressSpace("private")
case object LocalMemory extends AddressSpace("local")
case object GlobalMemory extends AddressSpace("global")

sealed abstract class Code
case class GeneratedCode(code: String, result: Variable, ty: Type) extends Code
case class ExpressionCode(expr: Expression) extends Code
case class PartialApplyCode(callee: Expression, args: List[Expression]) extends Code
case class AddressSpaceCode(code: PartialApplyCode, addressSpace: AddressSpace) extends Code

class Variable(val code: String) {
  override def toString: String = code

  def assign(idx: String, value: Variable) = s"$code[$idx] = ${value.code};"
  def assignSize(size: String) = ""
  def size(alt: String) = alt
}
object Variable {
  def apply(code: String) = new Variable(code)
  def unapply(arg: Variable): Option[String] = Some(arg.code)
}

case class DynArrayVariable(c: String, sizeVar: Variable) extends Variable(c) {
  override def assignSize(size: String): String = {
    if (sizeVar.code == "ary_size") {
      s"${sizeVar.code} = $size;"
    }
    else {
      s"int ${sizeVar.code} = $size;"
    }
  }
  override def size(alt: String): String = sizeVar.toString
}
