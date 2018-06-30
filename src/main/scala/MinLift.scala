
import Token.{Identifier, LParen}
import com.sun.javadoc.DocErrorReporter

import scala.io.Source
import scala.collection.immutable.Vector

object MinLift {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(args(0)).getLines().mkString("\n")
//    val tokens = tokenize("(+ 1 2)")
    val res = for (
//      tokens <- tokenize(source);
      tokens <- tokenize("(lift (N) ((array-type)))");
       ast <- parse(tokens)
    ) yield {
      println(tokens)
      println(ast)
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
            index -= 1
            if (source(index) == 'f') {
              Some(FloatLiteral(value.toFloat))
            }
            else {
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
      vars <- parseVariables();
      types <- parseInputTypes();
      _ <- consume(Token.RParen);
      _ <- eof()
    ) yield {
      new Lift(vars, types, Expression.Const[Int](1))
    }
  }

  def parseVariables(): Either[ErrType, Vector[Variable]] = {
    for (
      _ <- consume(Token.LParen);
      vars <- _while(false, tok => tok.isInstanceOf[Token.Identifier], tok => Right(Variable(tok.asInstanceOf[Token.Identifier].value)));
      _ <- consume(Token.RParen)
    ) yield {
      vars
    }
  }

  def parseInputTypes(): Either[ErrType, Vector[Type]] = {
    for (
      _ <- consume(Token.LParen);
      ty <- _while(true, tok => tok != Token.RParen, tok => parseInputType());
      _ <- consume(Token.RParen)
    ) yield {
      ty
    }
  }

  def parseInputType(): Either[ErrType, Type] = {
    for (
      _ <- consume(Token.LParen);
      _ <- consume(Token.Identifier("array-type"));
      _ <- consume(Token.RParen)
    ) yield {
      Type.Array(Type.Float, Variable("hoge"))
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

//  def consume[T](): Either[ErrType, Token] = {
//    val curr = tokens(pos)
//    if (curr.isInstanceOf[T]) {
//      Right(curr)
//    }
//    else {
//      Left(s"unexpected '${curr}', expected: '${classOf[T].getName}'")
//    }
//  }
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
    case class Apply(val callee: Expression, val args: List[Expression]) extends Expression

    case class Lambda(val args: List[Identifier], val body: Expression) extends Expression
    case class Map(val f: Lambda) extends Expression

    case class Identifier(val value: String) extends Expression
    case class Const[T](val value: T) extends Expression
  }
}


