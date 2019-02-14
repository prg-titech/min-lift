package parser

import scala.reflect.{ClassTag, classTag}

import token._
import ast._
import errors._

class Parser(val tokens: Vector[Token]) {
  var pos = 0

  type ErrType = LiftError
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

  def parseVariable(): Either[ErrType, Type.Size] = {
    consume[Identifier].map(tok => {
      Type.SizeVariable(tok.asInstanceOf[Identifier].value)
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
          case _ => Left(ParseError(s"unknown type ${value}", tok.pos))
        }
      }
    }
  }

  def parseExpression(): Either[ErrType, Expression] = {
    val expr = tokens(pos) match {
      case LParen => parseApplyAndSpecial()
      case cur@_ => {
        val expr = cur match {
          case Identifier(value) => Right(Expression.Identifier(value, false))
          case FloatLiteral(value) => Right(Expression.Const[Float](value))
          case DoubleLiteral(value) => Right(Expression.Const[Double](value))
          case IntLiteral(value) => Right(Expression.Const[Int](value))
          case BooleanLiteral(value) => Right(Expression.Const[Boolean](value))
          case SizeConstLiteral(value) => Right(Expression.Size(value))
          case _ => Left(ParseError(s"unknown token ${cur.getClass.getName}", cur.pos))
        }
        pos += 1
        expr
      }
    }
    expr
  }

  def parseApplyAndSpecial(): Either[ErrType, Expression] = {
    consume(LParen).flatMap(_ => {
      tokens(pos) match {
        case Identifier(fstId) => {
          fstId match {
            case "lambda" => {
              for (
                _ <- consume(Identifier("lambda"));
                args <- parseList(parseParam);
                body <- parseExpression();
                _ <- consume(RParen)
              ) yield {
                Expression.Lambda(args.toList, body)
              }
            }
            case "let" | "unpack" => {
              for (
                _ <- consume(Identifier(fstId));
                id <- parseIdentifier();
                value <- parseExpression();
                body <- parseExpression();
                _ <- consume(RParen)
              ) yield {
                Expression.Let(id, value, body, fstId == "unpack")
              }
            }
            case "pack" => {
              for(
                _ <- consume(Identifier(fstId));
                value <- parseExpression();
                _<- consume(RParen)
              ) yield {
                Expression.Pack(value)
              }
            }
            case _ => parseApply()
          }
        }
        case _ => parseApply()
      }
    })
  }

  def parseApply(): Either[ErrType, Expression.Apply] = {
    for (
      callee <- parseExpression();
      args <- _while(true, tok => tok != RParen, _ => parseExpression());
      _ <- consume(RParen)
    ) yield {
      Expression.Apply(callee, args.toList)
    }
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
    tokens.lift(pos).toRight(ParseError(s"unexpected EOF, expected: ${token}", tokens.last.pos)).flatMap(curr => {
      if (curr.equals(token)) {
        pos += 1
        Right(curr)
      }
      else {
        Left(ParseError(s"unexpected '${curr}', expected: '${token}'", curr.pos))
      }
    })
  }

  def consume[T <: Token : ClassTag](): Either[ErrType, Token] = {
    val klass = classTag[T].runtimeClass
    tokens.lift(pos).toRight(ParseError(s"unexpected EOF, expected: ${klass.getName}", tokens.last.pos)).flatMap(curr => {
      if (klass.isInstance(curr)) {
        pos += 1
        Right(curr)
      }
      else {
        Left(ParseError(s"unexpected '${curr}', expected: '${klass.getName}'", curr.pos))
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
      Left(ParseError("expected EOF", tokens.last.pos))
    }
    else {
      Right(Unit)
    }
  }
}

object Parser {
  def parse(tokens: Vector[Token]) = (new Parser(tokens)).parseLift()
}

