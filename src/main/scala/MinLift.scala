import Ast.Expression

import scala.io.Source
import scala.collection._
import scala.collection.immutable.Vector
import scala.reflect.{ClassTag, classTag}

object MinLift {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(args(0)).getLines().mkString("\n")
//    val tokens = tokenize("(+ 1 2)")
    val res = for (
       tokens <- tokenize(source);
       ast <- parse(tokens);
       _ <- (new TypeChecker).visit(ast);
       _ <- MemoryAllocator.inferAddressSpace(ast)
    ) yield {
      println(tokens)
      pprint.pprintln(ast)
      println("success checking type and allocating memory!")
      println((new PrintVisitor).visit(ast))
    }

    if (res.isLeft) {
      println(res.left.get)
    }
  }

  def tokenize(source: String): Either[String, Vector[Token]] = {
    import Token._

    var index = 0
    var tokens = Vector.empty[Token]

    val idRegex = """([a-zA-Z+\-\*\/$])""".r
    val headOfNumRegex = """([+\-0-9])""".r

    def readWhile(f: Char => Boolean): String = {
      var value = Vector.empty[Char]
      while (f(source(index))) {
        value :+= source(index)
        index += 1
      }
      value.mkString
    }

    while (index < source.length) {
      val token: Option[Token] = source(index) match {
        case '(' => Some(LParen)
        case ')' => Some(RParen)
        case '[' => Some(LBracket)
        case ']' => Some(RBracket)
        case idRegex(c) => {
          index += 1
          var value = c + readWhile(c => idRegex.findPrefixOf(c.toString).isDefined)
          index -= 1
          Some(Identifier(value))
        }
        case headOfNumRegex(c) => {
          index += 1
          var value = c + readWhile(c => c.isDigit)

          if (source(index) == '.') {
            index += 1
            value += '.' + readWhile(c => c.isDigit)
            if (source(index) == 'f') {
              Some(FloatLiteral(value.toFloat))
            }
            else {
              index -= 1
              Some(DoubleLiteral(value.toDouble))
            }
          }
          else {
            index -= 1
            Some(IntLiteral(value.toInt))
          }
        }
        case ' ' | '\r' | '\n' | '\t' => None
        case ';' => {
          readWhile(c => c != '\n')
          None
        }
        case _ => return Left(s"unknown token ${source(index)}")
      }

      token.map(tok => tokens :+= tok)

      index += 1
    }

    Right(tokens)
  }

  def parse(tokens: Vector[Token]): Either[String, Ast.Lift] = new Parser(tokens).parseLift
}

sealed abstract class Token
object Token {
  case object LParen extends Token
  case object RParen extends Token

  case object LBracket extends Token
  case object RBracket extends Token

  case class Identifier(val value: String) extends Token

  case class FloatLiteral(val value: Float) extends Token
  case class DoubleLiteral(val value: Double) extends Token
  case class IntLiteral(val value: Int) extends Token
}

class Parser(val tokens: Vector[Token]) {
  import Ast._

  var pos = 0

  type ErrType = String
  type ParseResult = Either[ErrType, Expression]

  def parseLift(): Either[ErrType, Lift] = {
    for (
      // TODO: LBracket, RBracketの組み合わせにも対応
      _ <- consume(Token.LParen);
      _ <- consume(Token.Identifier("lift"));
      vars <- parseList(parseVariable);
      types <- parseList(parseInputType);
      body <- parseExpression();
      _ <- consume(Token.RParen);
      _ <- eof()
    ) yield {
      new Lift(vars, types, body)
    }
  }

  def parseVariable(): Either[ErrType, Variable] = {
    consume[Token.Identifier].map(tok => {
      Variable(tok.asInstanceOf[Token.Identifier].value)
    })
  }

  def parseInputType(): Either[ErrType, Type] = {
    if (tokens(pos) == Token.LParen) {
      for (
        _ <- consume(Token.LParen);
        _ <- consume(Token.Identifier("array-type"));
        ty <- parseInputType();
        size <- parseVariable();
        _ <- consume(Token.RParen)
      ) yield {
        Type.Array(ty, size)
      }
    }
    else {
      consume[Token.Identifier].flatMap { tok =>
        val Token.Identifier(value) = tok
        value match {
          case "float" => Right(Type.Float)
          case _ => Left(s"unknown type ${value}")
        }
      }
    }
  }

  def parseExpression(): Either[ErrType, Expression] = {
    val expr = tokens(pos) match {
      case Token.LParen => parseApply()
      case cur@_ => {
        val expr = cur match {
          case Token.Identifier(value) => Right(Expression.Identifier(value, false))
          case Token.FloatLiteral(value) => Right(Expression.Const[Float](value))
          case Token.DoubleLiteral(value) => Right(Expression.Const[Double](value))
          case Token.IntLiteral(value) => Right(Expression.Const[Int](value))
          case _ => Left(s"unknown token ${cur.getClass.getName}")
        }
        pos += 1
        expr
      }
    }
    expr
  }

  def parseApply(): Either[ErrType, Expression] = {
    consume(Token.LParen).flatMap(_ => {
      tokens(pos) match {
        case Token.Identifier("lambda") => {
          for (
            _ <- consume(Token.Identifier("lambda"));
            args <- parseList(parseParam);
            body <- parseExpression();
            _ <- consume(Token.RParen)
          ) yield {
            Expression.Lambda(args, body)
          }
        }
        case _ => {
          for (
            callee <- parseExpression();
            args <- _while(true, tok => tok != Token.RParen, _ => parseExpression());
            _ <- consume(Token.RParen)
          ) yield {
            Expression.Apply(callee, args)
          }
        }
      }
    })
  }

  def parseIdentifier(): Either[ErrType, Expression.Identifier] = {
    consume[Token.Identifier].map(tok => {
      val Token.Identifier(value) = tok
      Expression.Identifier(value, false)
    })
  }

  def parseParam(): Either[ErrType, Expression.Identifier] = {
    consume[Token.Identifier].map(tok => {
      val Token.Identifier(value) = tok
      Expression.Identifier(value, true)
    })
  }

  def parseList[A](parser: () => Either[ErrType, A]): Either[ErrType, Vector[A]] = {
    for (
      _ <- consume(Token.LParen);
      nodes <- _while(true, tok => tok != Token.RParen, tok => parser());
      _ <- consume(Token.RParen)
    ) yield {
      nodes
    }
  }

  def consume(token: Token): Either[ErrType, Token] = {
    val curr = tokens(pos)
    if (curr.equals(token)) {
      pos += 1
      Right(curr)
    }
    else {
      Left(s"unexpected '${curr}', expected: '${token}'")
    }
  }

  def consume[T <: Token : ClassTag](): Either[ErrType, Token] = {
    val curr = tokens(pos)
    val klass = classTag[T].runtimeClass
    if (klass.isInstance(curr)) {
      pos += 1
      Right(curr)
    }
    else {
      Left(s"unexpected '${curr}', expected: '${klass.getName}'")
    }
  }

  def _while[T](nest: Boolean, cond: Token => Boolean, f: Token => Either[ErrType, T]): Either[ErrType, Vector[T]] = {
    var res = Vector.empty[T]
    while(cond(tokens(pos))) {
      f(tokens(pos)) match {
        case Right(tok) => res :+= tok
        case Left(err) => return Left(err)
      }
      if (!nest) {
        pos += 1
      }
    }
    Right(res)
  }

  def eof(): Either[ErrType, Unit] = {
    if (pos < tokens.length) {
      Left("expected EOF")
    }
    else {
      Right(Unit)
    }
  }
}

object Ast {
  case class Lift(val variables: Vector[Variable], val inputTypes: Vector[Type], val body: Expression) {
    def accept[T](visitor: Visitor[T]): Unit = {
      visitor.visit(this)
    }
  }

  sealed abstract class Type
  object Type {
    case class Array(val innerType: Type, val length: Variable) extends Type {
      override def toString: String = s"${innerType}[${length}]"
    }
    case class Function(val argTypes: Vector[Type], val resultType: Type) extends Type {
      override def toString: String = s"(${argTypes.mkString(", ")}) => ${resultType}"
    }
    class Scalar extends Type
    case object Float extends Scalar
    case object Double extends Scalar
    case object Int extends Scalar

    // TODO: Add vector type and tuple type

    case class Polymorphic(val name: String) extends Type {
      override def toString: String = s"<${name}>"
    }

    case object Unfixed extends Type
  }

  case class Variable(val name: String) {
    override def toString: String = name
  }

  sealed abstract class Expression {
    var ty: Type = Type.Unfixed
    var addressSpace: Option[MemoryAllocator.AddressSpace] = None

    def accept[T](visitor: Visitor[T]): T
  }
  object Expression {
    case class Apply[U](val callee: Expression, val args: Vector[U]) extends Expression {
      def accept[T](visitor: Visitor[T]): T = {
        visitor.visit(this)
      }
    }

    case class Lambda(val args: Vector[Identifier], val body: Expression) extends Expression {
      def accept[T](visitor: Visitor[T]): T = {
        visitor.visit(this)
      }
    }

    case class Map(val f: Expression) extends Expression {
      def accept[T](visitor: Visitor[T]): T = {
        visitor.visit(this)
      }
    }

    case class Identifier(val value: String, val isParam: Boolean) extends Expression {
      def accept[T](visitor: Visitor[T]): T = {
        visitor.visit(this)
      }
    }

    case class Const[U](val value: U) extends Expression {
      def accept[T](visitor: Visitor[T]): T = {
        visitor.visit(this)
      }
    }

    case class Undefined() extends Expression {
      def accept[T](visitor: Visitor[T]): T = {
        visitor.visit(this)
      }
    }
  }
}

trait Visitor[T] {
  def visit(node: Ast.Lift): T
  def visit[U](node: Ast.Expression.Apply[U]): T
  def visit(node: Ast.Expression.Lambda): T
  def visit(node: Ast.Expression.Map): T
  def visit(node: Ast.Expression.Identifier): T
  def visit[U](node: Ast.Expression.Const[U]): T
  def visit(node: Ast.Expression.Undefined): T
}

// check and infer types by abstract interpretation
class TypeChecker extends Visitor[Either[String, (Ast.Expression, Ast.Type)]] {
  type Value = (Ast.Expression, Ast.Type)
  type TypeCheckerResult = Either[String, Value]
  type LocalEnv = mutable.Map[String, Value]

  val env = mutable.Stack[LocalEnv]()
  env.push(mutable.Map[String, Value]())

  override def visit(node: Ast.Lift): TypeCheckerResult = {
    // set default definitions
    // mapSeq :: (A => B) => (A[]n => B[]n)
    env.top += "mapSeq" -> (
      Ast.Expression.Undefined(),
      Ast.Type.Function(
        Vector(Ast.Type.Function(
          Vector(Ast.Type.Polymorphic("A")),
          Ast.Type.Polymorphic("B"))),
        Ast.Type.Function(
          Vector(Ast.Type.Array(Ast.Type.Polymorphic("A"), Ast.Variable("N"))),
          Ast.Type.Array(Ast.Type.Polymorphic("B"), Ast.Variable("N")))))
    env.top += "*" -> (
      Ast.Expression.Undefined(),
      Ast.Type.Function(
        Vector(Ast.Type.Float, Ast.Type.Float),
        Ast.Type.Float))

    val root = node.body.accept(this)
    root.flatMap(root => {
      Ast.Expression.Apply(root._1, node.inputTypes).accept(this)
    })
  }

  override def visit[U](node: Ast.Expression.Apply[U]): TypeCheckerResult = {
    node.callee.accept(this).flatMap(callee => {
      // convert Vector[Either[E, X]] to Either[E, Vector[X]]
      val actualArgs = node.args
        .map{
          case arg:Ast.Expression => arg.accept(this)
          case arg:Ast.Type => Right((Ast.Expression.Undefined(), arg))
        }
        .foldLeft(Right(Vector.empty[Value]) : Either[String, Vector[Value]])((args, arg) => {
          arg match {
            case Right(t) => args.map(_ :+ t)
            case Left(err) => Left(err)
          }
        })

      // check types passed to the function
      actualArgs.flatMap(actualArgs => {
        callee._2 match {
          case Ast.Type.Function(expectedArgTypes, expectedResultType) => {
            val polymorphicMap = mutable.Map[String, Ast.Type]()

            def equalsType(a: Ast.Type, b : Ast.Type): Boolean = (a, b) match {
              case (Ast.Type.Array(aInner, aLength), Ast.Type.Array(bInner, bLength)) => {
                aLength.equals(bLength) && satisfiedType(aInner, bInner)
              }
              case (Ast.Type.Function(aArgs, aResult), Ast.Type.Function(bArgs, bResult)) => {
                aArgs.zip(bArgs).forall { case (aty, bty) => satisfiedType(aty, bty) } && satisfiedType(aResult, bResult)
              }
              case (Ast.Type.Float, Ast.Type.Float) => true
              case _ => false
            }

            def resolveType(ty: Ast.Type): Ast.Type = ty match {
              case Ast.Type.Array(innerType, length) => Ast.Type.Array(resolveType(innerType), length)
              case Ast.Type.Polymorphic(x) => {
                polymorphicMap.get(x).getOrElse(Ast.Type.Polymorphic(s"Unresolved ${x}"))
              }
              case Ast.Type.Unfixed => Ast.Type.Unfixed
              case _ => ty
            }

            def satisfiedType(actual: Ast.Type, expected: Ast.Type): Boolean = {
              (actual, expected) match {
                case (Ast.Type.Unfixed, _) => {
                  // check later
                  true
                }
                case (_, Ast.Type.Unfixed) => true
                case (_, Ast.Type.Polymorphic(x)) => {
                  polymorphicMap.get(x) match {
                    case Some(ty) => equalsType(actual, ty)
                    case None => {
                      polymorphicMap += x -> actual
                      true
                    }
                  }
                }
                case _ => equalsType(actual, expected)
              }
            }

            // resolve unfixed or polymorphic types of the function
            val satisfiedArgTypes = actualArgs
              .zip(expectedArgTypes)
              .forall { case (actual, expected) => satisfiedType(actual._2, expected) }

            if (satisfiedArgTypes) {
              val result = callee._1 match {
                case Ast.Expression.Lambda(argIds, body) => {
                  val local = mutable.Map[String, Value]()
                  argIds.zip(actualArgs).foreach { case (id, arg) =>
                    local += id.value -> arg
                  }

                  env.push(local)
                  val result = body.accept(this)
                  env.pop()
                  result
                }
                case Ast.Expression.Map(f) => {
                  val Ast.Type.Array(innerType, length) = actualArgs(0)._2
                  val result = Ast.Expression.Apply(f, Vector(innerType)).accept(this)

                  result.map { case (_, ty) =>
                    f.ty = Ast.Type.Function(Vector(innerType), ty)

                    (Ast.Expression.Undefined(), Ast.Type.Array(ty, length))
                  }
                }
                case Ast.Expression.Undefined() => {
                  // built-in function
                  val Ast.Expression.Identifier (name, _) = node.callee
                  val result = name match {
                    case "mapSeq" => {
                      val f = actualArgs (0)._1
                      Ast.Expression.Map (f)
                    }
                    case "*" => {
                      Ast.Expression.Undefined()
                    }
                  }

                  Right ((result, expectedResultType))
                }
              }

              result.flatMap { case (expr, actualResultType) =>
                val satisfiedResultType = satisfiedType(actualResultType, expectedResultType)
                if (satisfiedResultType) {

                  node.callee.ty = Ast.Type.Function(
                    expectedArgTypes.map(ty => resolveType(ty)),
                    resolveType(actualResultType)
                  )
                  node.ty = actualResultType

                  Right((expr, actualResultType))
                }
                else {
                  Left(s"result type mismatch. expected ${expectedResultType}, actual: ${actualResultType}")
                }
              }
            }
            else {
              Left(s"argument types mismatch. expected ${expectedArgTypes}, actual: ${actualArgs.map(_._2)}")
            }
          }
          case _ => {
            Left(s"callee(${callee}) is not function")
          }
        }
      })
    })
  }

  override def visit(node: Ast.Expression.Lambda): TypeCheckerResult= {
    val ty = Ast.Type.Function(node.args.map(_ => Ast.Type.Unfixed), Ast.Type.Unfixed)
    node.ty = ty
    Right((node, ty))
  }

  override def visit(node: Ast.Expression.Map): TypeCheckerResult = ???

  override def visit(node: Ast.Expression.Identifier): TypeCheckerResult = {
    for (local <- env) {
      local.get(node.value) match {
        case Some(value) => {
          node.ty = value._2
          return Right(value)
        }
        case None => ()
      }
    }

    Left(s"undefined variable ${node.value}")
  }

  override def visit[U](node: Ast.Expression.Const[U]): TypeCheckerResult = node match {
    case Ast.Expression.Const(value) => {
      val ty = value match {
        case v:Float => Ast.Type.Float
        case v:Double => Ast.Type.Double
        case v:Int => Ast.Type.Int
      }
      node.ty = ty
      Right((node, ty))
    }
  }

  override def visit(node: Ast.Expression.Undefined): TypeCheckerResult = ???
}

class PrintVisitor extends Visitor[String] {
  def pad(str: String): String = {
    str.split("\n").map(l => s"  ${l}").mkString("\n")
  }

  override def visit(node: Ast.Lift): String = {
    s"""
       |(lift
       |  (${node.variables.mkString(", ")})
       |  (${node.inputTypes.mkString(", ")})${pad(node.body.accept(this))})""".stripMargin
  }

  override def visit[U](node: Ast.Expression.Apply[U]): String = {
    val args = node.args.map {
      case arg:Ast.Expression => arg.accept(this)
      case arg:Ast.Type => arg.toString
    }.mkString(" ")
    s"""
       |(${node.callee.accept(this)}${pad(args)}): ${node.ty}""".stripMargin
  }

  override def visit(node: Ast.Expression.Lambda): String = {
    s"""
       |(lambda
       |  (${node.args.map(_.accept(this)).mkString(" ")})${pad(node.body.accept(this))}): ${node.ty}""".stripMargin
  }

  override def visit(node: Ast.Expression.Map): String = ???

  override def visit(node: Ast.Expression.Identifier): String = {
    s"${node.value}: ${node.ty}@${node.addressSpace}"
  }

  override def visit[U](node: Ast.Expression.Const[U]): String = {
    s"${node.value}: ${node.ty}@${node.addressSpace}"
  }

  override def visit(node: Ast.Expression.Undefined): String = {
    "undefined"
  }
}

// Allocation memory to expression by P79 Algorithm 1
// in "Lift: A Functional Data-Parallel IR for High-Performance GPU Code Generation"
//    Michel Steuwer, Toomas Remmelg, Christophe Dubach
object MemoryAllocator  {
  sealed abstract class AddressSpace
  case object PrivateMemory extends AddressSpace
  case object LocalMemory extends AddressSpace
  case object GlobalMemory extends AddressSpace

  def inferAddressSpace(lift: Ast.Lift): Either[Unit, Unit] = {
    val Ast.Expression.Lambda(params, body) = lift.body
    lift.inputTypes.zip(params).foreach { case (inputType, param) =>
      if (inputType.isInstanceOf[Ast.Type.Scalar]) {
        param.addressSpace = Some(PrivateMemory)
      }
      else {
        param.addressSpace = Some(GlobalMemory)
      }
    }
    inferAddressSpaceExpr(body, None)

    Right(())
  }

  def inferAddressSpaceExpr(expr: Ast.Expression, writeTo: Option[AddressSpace]): Unit = {
    import Ast.Expression._

    println("inferASExpr", expr, writeTo)

    expr match {
      case Const(_) => expr.addressSpace = Some(PrivateMemory)
      case Identifier(_, true) /* as Param */ => assert(expr.addressSpace != None)
      case Apply(f, argsAny) => {
        val args = argsAny.map { case arg:Expression => arg }

        args.foreach(arg => inferAddressSpaceExpr(arg, writeTo))

        f match {
          // TODO implement following expressions
          // case UserFun
          // case toPrivate
          // case toLocal
          // case toGlobal
          // case Reduce
          // case Iterate
          case l@Lambda(_, _) => inferAddressSpaceApply(l, args, writeTo)
          // case Map(l@Lambda(_, _)) => inferAddressSpaceApply(l, args, writeTo)
          case Identifier("mapSeq", false) => inferAddressSpaceExpr(args(0), writeTo)
          case Apply(_, _) => inferAddressSpaceExpr(f, writeTo)
          case _ => expr.addressSpace = args.lift(0).flatMap(_.addressSpace) // not args.addressSpace
        }
      }
      case Lambda(_, body) => inferAddressSpaceExpr(body, writeTo)
      case _ => {}
    }
  }

  def inferAddressSpaceApply(lambda: Ast.Expression.Lambda, args: Vector[Expression], writeTo: Option[AddressSpace]): Unit = {
    lambda.args.zip(args).foreach { case (p, a) => p.addressSpace = a.addressSpace }
    inferAddressSpaceExpr(lambda.body, writeTo)
  }
}