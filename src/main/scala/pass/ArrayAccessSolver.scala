package pass

import scala.collection._
import scala.collection.immutable.List
import ast._
import structures._

class ArrayAccessSolver extends ExpressionVisitor[Environment[View], Unit] {
  val stack = new mutable.ArrayStack[View]

  var indexVarCount = 0
  def makeIndexVar = {
    indexVarCount += 1
    indexVarCount
  }

  def visit(node: Lift, env: ArgumentType): ResultType = {
    // val env2 = node.body.args.foldRight(env)((id, env) => env.append(id.value, MemoryView(id.value)))
    // node.body.accept(this, env2)
    node.body.args.foreach(arg => stack.push(MemoryView(arg.value)))
    node.body.accept(this, env)
  }

  override def visit(node: Expression.Apply, env: ArgumentType): ResultType = {
    node.callee match {
      case Expression.Identifier(name, false) => {
        name match {
          case "zip" => {
            val args = node.args.map(arg => {
              arg.accept(this, env)
              stack.pop()
            })
            stack.push(ZipView(args))
          }
          case "split" => {
            val Expression.Size(size) = node.args(0)
            node.args.lift(1).foreach(_.accept(this, env))
            stack.push(SplitView(size, stack.pop()))
          }
          case "mapLcl" | "mapWrg" | "mapGlb" => {
            node.args.lift(1).foreach(_.accept(this, env))
            val res = ArrayAccessView(makeIndexVar, stack.pop())
            node.view = res
            stack.push(res)
            node.args(0).accept(this, env)
          }
          case "mapSeq" => {
            node.args.lift(1).foreach(_.accept(this, env))
            val res = ArrayAccessView(makeIndexVar, stack.pop())
            stack.push(NullView())
            node.view = res
            stack.push(res)
            node.args(0).accept(this, env)
          }
          case "reduceSeq" => {
            node.args.lift(2).foreach(_.accept(this, env))
            val res = ArrayAccessView(makeIndexVar, stack.pop())
            stack.push(NullView())
            node.view = res
            stack.push(res)
            node.args(1).accept(this, env)
          }
          case "filterSeq" => {
            node.args.lift(1).foreach(_.accept(this, env))
            val res = ArrayAccessView(makeIndexVar, stack.pop())
            node.view = res
            stack.push(res)
            node.args(0).accept(this, env)
          }
          case "get1" | "get2" => {
            val index = if (name == "get1") { 0 } else { 1 }
            node.args(0).accept(this, env)
            val res = TupleAccessView(index, stack.pop())
            node.view = res
            stack.push(res)
          }
          case _ => {
            node.args.foreach(node => {
              node.accept(this, env)
              stack.pop()
            })
            stack.push(NullView())
          }
        }
      }
    }
    // node.view = result
//    stack.push(result)
    // node.callee.accept(this, env)
  }

  override def visit(node: Expression.Lambda, env: ArgumentType): ResultType = {
    val env2 = node.args.foldRight(env.pushEnv(scala.Predef.Map[String, View]()))((id, env) => env.append(id.value, stack.pop()))
    node.body.accept(this, env2)
  }

  override def visit(node: Expression.Identifier, env: ArgumentType): ResultType = {
    stack.push(env.lookup(node.value).get)
  }

  override def visit[C](node: Expression.Const[C], env: ArgumentType): ResultType = {
    // do nothing
    stack.push(NullView())
  }

  override def visit(node: Expression.Size, env: ArgumentType): ResultType = {
    // do nothing
    stack.push(NullView())
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

trait ViewVisitor[A, R] {
  type ArgumentType = A
  type ResultType = R

  def visit(view: ArrayAccessView, arg: A): R
  def visit(view: SplitView, arg: A): R
  def visit(view: MemoryView, arg: A): R
  def visit(view: ZipView, arg: A): R
  def visit(view: TupleAccessView, arg: A): R
  def visit(view: NullView, arg: A): R
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

  def visit(view: NullView, stacks: ArgumentType) = stacks match {
    case (arrayStack, tupleStack) =>
      Left("Encounter NullView.")
  }
}

object ViewConstructor {
  def construct(view: View) = {
    view.accept(new ViewConstructor, (List(), List())).flatMap{ case (arrayStack, _) =>
      arrayStack match {
        case ViewExpression.ArrayVariable(name) :: expr :: _ => {
          Right(s"$name[$expr]")
        }
        case _ => {
          Left(s"Array stack of result must have one expr and one array variable.\narrayStack = ${arrayStack}")
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

