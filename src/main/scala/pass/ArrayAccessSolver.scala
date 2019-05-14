package pass

import scala.collection._
import scala.collection.immutable.List
import ast._
import lib._

class ArrayAccessSolver extends ExpressionVisitor[Environment[View], View] {

  val indexVarGen = new UniqueIdGenerator()
  val interimResultVarGen = new UniqueIdGenerator()

  val funcs = lift.BuiltInFunctions.getFuncs(new UniqueIdGenerator())

  def visit(node: Lift, env: ArgumentType): ResultType = {
    val Expression.Lambda(ids, body) = node.body
    val args = ids.map(id => MemoryView(id.value))
    evalApplyLambda(node.body, args, env)
  }

  override def visit(node: Expression.Apply, env: ArgumentType): ResultType = {
    val callee = node.callee.accept(this, env)
    val args = node.args.map(_.accept(this, env))
    val view = evalApply(callee, args, env)
    node.view = view
    view
  }

  def evalApply(callee: View, aargs: List[View], env: ArgumentType): ResultType = {
    callee match {
      case ExpressionView(callee) => {
        evalApply(PartialApplyView(callee, aargs), List(), env)
      }
      case PartialApplyView(callee, pargs) => {
        val args = pargs ::: aargs

        callee match {
          case Expression.Identifier(name, false) => {

            funcs.lookup(name) match {
              case Some(func) => {
                val arw@Type.Arrow(_, _) = func.ty
                val argCount = arw.args.size

                if (argCount != args.size) {
                  return PartialApplyView(callee, args)
                }
              }
              case _ => {}
            }

            name match {
              case "zip" => {
                ZipView(args)
              }
              case "split" => {
                val ExpressionView(Expression.Size(size)) = args(0)
                SplitView(size, args(1))
              }
              case "mapSeq" | "mapLcl" | "mapWrg" | "mapGlb" => {
                evalApply(args(0), List(ArrayAccessView(indexVarGen.generateInt(), args(1))), env)
                MemoryView(s"temp${interimResultVarGen.generateInt()}")
              }
              case "reduceSeq" => {
                evalApply(args(1), List(NullView(), ArrayAccessView(indexVarGen.generateInt(), args(2))), env)
              }
              case "filterSeq" | "filterGlb" => {
                evalApply(args(0), List(ArrayAccessView(indexVarGen.generateInt(), args(1))), env)
                MemoryView(s"temp${interimResultVarGen.generateInt()}")
              }
              case "get1" | "get2" => {
                val index = if (name == "get1") { 0 } else { 1 }
                TupleAccessView(index, args(0))
              }
              case "toGlobal" | "toLocal" | "toPrivate" => {
                args(0)
              }
              case _ => {
                NullView()
              }
            }
          }
          case lambda@Expression.Lambda(largs, body) => {
            if (largs.size == args.size) {
              evalApplyLambda(lambda, args, env)
            }
            else {
              PartialApplyView(lambda, args)
            }
          }
        }
      }
      case NullView() => {
        NullView()
      }
    }
  }

  def evalApplyLambda(lambda: Expression.Lambda, args: List[View], env: ArgumentType): ResultType = {
    val env2 = lambda.args.zip(args).foldRight(env.pushEnv(scala.Predef.Map[String, View]())){ case ((id, view), env) => {
      env.append(id.value, view)
    }}

    lambda.body.accept(this, env2)
  }

  override def visit(node: Expression.Lambda, arg: ArgumentType): ResultType = {
    PartialApplyView(node, List())
  }

  override def visit(node: Expression.Let, env: ArgumentType): ResultType = {
    val env2 = env.pushEnv(Map(node.id.value -> node.value.accept(this, env)))
    node.body.accept(this, env2)
  }

  override def visit(node: Expression.Pack, env: ArgumentType): ResultType = {
    node.value.accept(this, env)
  }

  override def visit(node: Expression.Identifier, env: ArgumentType): ResultType = {
    env.lookup(node.value) match {
      case Some(view) => {
        node.view = view
        view
      }
      case None => {
        ExpressionView(node)
      }
    }
  }

  override def visit[C](node: Expression.Const[C], arg: ArgumentType): ResultType = {
    ExpressionView(node)
  }

  override def visit(node: Expression.Size, arg: ArgumentType): ResultType = {
    ExpressionView(node)
  }
}

object ArrayAccessSolver {
  def solve(node: Lift) = (new ArrayAccessSolver).visit(node, EmptyEnvironment[View]())
}

sealed abstract class View {
  def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R
}
case class ArrayAccessView(id: Int, child: View) extends View {
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}
case class SplitView(size: Int, child: View) extends View {
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}
case class MemoryView(varName: String) extends View{
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}
case class ZipView(children: List[View]) extends View {
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}
case class TupleAccessView(index: Int, child: View) extends View {
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}
case class NullView() extends View {
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}
// for constructing view
case class PartialApplyView(callee: Expression, args: List[View]) extends View {
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}
case class ExpressionView(expr: Expression) extends View {
  override def accept[A, R](visitor: ViewVisitor[A, R], arg: A): R = visitor.visit(this, arg)
}

trait ViewVisitor[A, R] {
  type ArgumentType = A
  type ResultType = R

  def visit(view: ArrayAccessView, arg: A): R
  def visit(view: SplitView, arg: A): R
  def visit(view: MemoryView, arg: A): R
  def visit(view: ZipView, arg: A): R
  def visit(view: TupleAccessView, arg: A): R
  def visit(view: NullView, arg: A): R
  def visit(view: PartialApplyView, arg: A): R
  def visit(view: ExpressionView, arg: A): R
}

class ViewConstructor extends ViewVisitor[(List[ViewExpression], List[Int]), Either[String, (List[ViewExpression], List[Int])]] {

  import ViewExpression._

  def visit(view: ArrayAccessView, stacks: ArgumentType) = stacks match {
    case (arrayStack, tupleStack) =>
      view.child.accept(this, (Variable(view.id) :: arrayStack, tupleStack))
  }

  def visit(view: SplitView, stacks: ArgumentType) = stacks match {
    case (arrayStack, tupleStack) =>
      arrayStack match {
        case index :: rest => {
          val expr0 = BinaryOp(IntLiteral(view.size), Mult, index)
          val (expr, arrayStack) = rest match {
            case offset :: rest => {
              (BinaryOp(expr0, Plus, offset), rest)
            }
            case _ => {
              (expr0, rest)
            }
          }
          view.child.accept(this, (expr :: arrayStack, tupleStack))
        }
        case _ => {
          Left("Array Stack is empty.")
        }
      }
  }

  def visit(view: MemoryView, stacks: ArgumentType) = stacks match {
    case (arrayStack, tupleStack) =>
      Right((ArrayVariable(view.varName) :: arrayStack, tupleStack))
  }

  def visit(view: ZipView, stacks: ArgumentType) = stacks match {
    case (arrayStack, tupleStack) =>
      tupleStack match {
        case index :: tupleStack => {
          view.children(index).accept(this, (arrayStack, tupleStack))
        }
        case _ => {
          Left("Tuple Stack is empty.")
        }
      }
  }

  def visit(view: TupleAccessView, stacks: ArgumentType) = stacks match {
    case (arrayStack, tupleStack) =>
      view.child.accept(this, (arrayStack, view.index :: tupleStack))
  }

  def visit(view: NullView, stacks: ArgumentType) = Left("Encounter NullView.")

  def visit(view: PartialApplyView, stacks: ArgumentType) = Left("Encounter PartialApplyView.")
  def visit(view: ExpressionView, stacks: ArgumentType) = Left("Encounter ExpressionView.")
}

object ViewConstructor {
  def construct(view: View) = {
    view.accept(new ViewConstructor, (List(), List())).flatMap{ case (arrayStack, _) =>
      arrayStack match {
        case ViewExpression.ArrayVariable(name) :: expr :: _ => {
          Right(s"$name[$expr]")
        }
        case _ => {
          Left(s"Array stack of result must have one expr and one array variable. arrayStack = ${arrayStack}")
        }
      }
    }
  }
}

sealed abstract class ViewExpression {
  def toString: String
}
object ViewExpression {
  case class Variable(id: Int) extends ViewExpression {
    override def toString: String = s"i${id.toString}"
  }
  case class BinaryOp(left: ViewExpression, op: Operation, right: ViewExpression) extends ViewExpression {
    override def toString: String = s"$left $op $right"
  }
  case class IntLiteral(value: Int) extends ViewExpression {
    override def toString: String = value.toString
  }

  case class ArrayVariable(name: String) extends ViewExpression {
    override def toString: String = name
  }

  class Operation(str: String) {
    override def toString: String = str
  }
  case object Plus extends Operation("+")
  case object Mult extends Operation("*")
}

