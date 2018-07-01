import Token.Identifier

import scala.io.Source
import scala.collection.immutable.Vector
import scala.reflect.{ClassTag, classTag}

object MinLift {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(args(0)).getLines().mkString("\n")
//    val tokens = tokenize("(+ 1 2)")
    val res = for (
       tokens <- tokenize(source);
       ast <- parse(tokens)
    ) yield {
      println(tokens)
      pprint.pprintln(ast)
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
          case Token.Identifier(value) => Right(Expression.Identifier(value))
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
            args <- parseList(parseIdentifier);
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
      Expression.Identifier(value)
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
  case class Lift(val variables: Vector[Variable], val inputTypes: Vector[Type], val body: Expression)

  sealed abstract class Type
  object Type {
    case class Array(val innerType: Type, val length: Variable) extends Type
    case object Float extends Type
  }

  case class Variable(val name: String)

  sealed abstract class Expression
  object Expression {
    case class Apply(val callee: Expression, val args: Vector[Expression]) extends Expression

    case class Lambda(val args: Vector[Identifier], val body: Expression) extends Expression
//    case class Map(val f: Expression) extends Expression

    case class Identifier(val value: String) extends Expression
    case class Const[T](val value: T) extends Expression
  }
}


