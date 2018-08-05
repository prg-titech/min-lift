package parser

import scala.reflect.{ClassTag, classTag}

import token._
import ast._

class Parser(val tokens: Vector[Token]) {
  var pos = 0

  type ErrType = String
  type ParseResult = Either[ErrType, Expression]

  def parseLift(): Either[ErrType, Lift] = {
    for (
      // TODO: LBracket, RBracketの組み合わせにも対応
      _ <- consume(LParen);
      _ <- consume(Identifier("lift"));
      vars <- parseList(parseVariable);
      types <- parseList(parseInputType);
      body <- parseExpression();
      _ <- consume(RParen);
      _ <- eof()
    ) yield {
      new Lift(vars, types, body.asInstanceOf[Expression.Lambda])
    }
  }

  def parseVariable(): Either[ErrType, Variable] = {
    consume[Identifier].map(tok => {
      Variable(tok.asInstanceOf[Identifier].value)
    })
  }

  def parseInputType(): Either[ErrType, Type] = {
    if (tokens(pos) == LParen) {
      for (
        _ <- consume(LParen);
        _ <- consume(Identifier("array-type"));
        ty <- parseInputType();
        size <- parseVariable();
        _ <- consume(RParen)
      ) yield {
        Type.Array(ty, size)
      }
    }
    else {
      consume[Identifier].flatMap { tok =>
        val Identifier(value) = tok
        value match {
          case "float" => Right(Type.Float)
          case _ => Left(s"unknown type ${value}")
        }
      }
    }
  }

  def parseExpression(): Either[ErrType, Expression] = {
    val expr = tokens(pos) match {
      case LParen => parseApply()
      case cur@_ => {
        val expr = cur match {
          case Identifier(value) => Right(Expression.Identifier(value, false))
          case FloatLiteral(value) => Right(Expression.Const[Float](value))
          case DoubleLiteral(value) => Right(Expression.Const[Double](value))
          case IntLiteral(value) => Right(Expression.Const[Int](value))
          case _ => Left(s"unknown token ${cur.getClass.getName}")
        }
        pos += 1
        expr
      }
    }
    expr
  }

  def parseApply(): Either[ErrType, Expression] = {
    consume(LParen).flatMap(_ => {
      tokens(pos) match {
        case Identifier("lambda") => {
          for (
            _ <- consume(Identifier("lambda"));
            args <- parseList(parseParam);
            body <- parseExpression();
            _ <- consume(RParen)
          ) yield {
            Expression.Lambda(args, body)
          }
        }
        case _ => {
          for (
            callee <- parseExpression();
            args <- _while(true, tok => tok != RParen, _ => parseExpression());
            _ <- consume(RParen)
          ) yield {
            Expression.Apply(callee, args)
          }
        }
      }
    })
  }

  def parseIdentifier(): Either[ErrType, Expression.Identifier] = {
    consume[Identifier].map(tok => {
      val Identifier(value) = tok
      Expression.Identifier(value, false)
    })
  }

  def parseParam(): Either[ErrType, Expression.Identifier] = {
    consume[Identifier].map(tok => {
      val Identifier(value) = tok
      Expression.Identifier(value, true)
    })
  }

  def parseList[A](parser: () => Either[ErrType, A]): Either[ErrType, Vector[A]] = {
    for (
      _ <- consume(LParen);
      nodes <- _while(true, tok => tok != RParen, tok => parser());
      _ <- consume(RParen)
    ) yield {
      nodes
    }
  }

  def consume(token: Token): Either[ErrType, Token] = {
    tokens.lift(pos).toRight(s"unexpected EOF, expected: ${token}").flatMap(curr => {
      if (curr.equals(token)) {
        pos += 1
        Right(curr)
      }
      else {
        Left(s"unexpected '${curr}', expected: '${token}'")
      }
    })
  }

  def consume[T <: Token : ClassTag](): Either[ErrType, Token] = {
    val klass = classTag[T].runtimeClass
    tokens.lift(pos).toRight(s"unexpected EOF, expected: ${klass.getName}").flatMap(curr => {
      if (klass.isInstance(curr)) {
        pos += 1
        Right(curr)
      }
      else {
        Left(s"unexpected '${curr}', expected: '${klass.getName}'")
      }
    })
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

object Parser {
  def parse(tokens: Vector[Token]) = (new Parser(tokens)).parseLift()
}
