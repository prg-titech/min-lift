package token

import errors._

object Token {
  def tokenize(source: String): Either[LexerError, Vector[Token]] = {
    var index = 0
    var tokens = Vector.empty[Token]

    var line = 0
    var col = 0

    val idRegex = """([a-zA-Z+\-\*\/$])""".r
    val headOfNumRegex = """([+\-0-9])""".r

    def readWhile(f: Char => Boolean): String = {
      var value = Vector.empty[Char]
      while (index < source.size && f(source(index))) {
        value :+= source(index)
        index += 1
      }
      value.mkString
    }

    def pos: Position = {
      Position(line, col)
    }

    while (index < source.length) {
      val prevIndex = index

      var newLine = false

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
        case '#' => {
          index += 1
          var value = readWhile(c => c.isDigit)
          index -= 1
          Some(SizeConstLiteral(value.toInt))
        }
        case ' ' | '\r' | '\t' => None
        case '\n' => {
          newLine = true
          None
        }
        case ';' => {
          newLine = true
          readWhile(c => c != '\n')
          None
        }
        case _ => return Left(LexerError(s"unknown character ${source(index)}", pos))
      }

      token.map{ tok =>
        tok.pos = pos
        tokens :+= tok
      }

      index += 1

      col += index - prevIndex

      if (newLine) {
        line += 1
        col = 0
      }
    }

    Right(tokens)
  }
}

sealed abstract class Token {
  var pos = Position(0, 0)
}
case object LParen extends Token
case object RParen extends Token
case object LBracket extends Token
case object RBracket extends Token
case class Identifier(val value: String) extends Token
case class FloatLiteral(val value: Float) extends Token
case class DoubleLiteral(val value: Double) extends Token
case class IntLiteral(val value: Int) extends Token
case class SizeConstLiteral(val value: Int) extends Token

