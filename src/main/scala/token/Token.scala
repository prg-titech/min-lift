package token

sealed abstract class Token

object Token {
  def tokenize(source: String): Either[String, Vector[Token]] = {
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
}

case object LParen extends Token
case object RParen extends Token
case object LBracket extends Token
case object RBracket extends Token
case class Identifier(val value: String) extends Token
case class FloatLiteral(val value: Float) extends Token
case class DoubleLiteral(val value: Double) extends Token
case class IntLiteral(val value: Int) extends Token

