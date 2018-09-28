package errors

case class Position(val line: Int, val column: Int) {
  override def toString: String = s"line: ${line + 1}, column: ${column + 1}"
}

class LiftError

case class LexerError(val message: String, val position: Position) extends LiftError {
  override def toString: String = s"$message at $position"
}
case class ParseError(val message: String, val position: Position) extends LiftError {
  override def toString: String = s"$message at $position"
}
