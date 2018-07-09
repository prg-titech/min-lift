import org.scalatest.FunSpec
import token._
import parser._
import ast._
import pass.TypeChecker

class MinLiftSpec extends FunSpec {

  val code =
        """
          |(lift
          | (N)
          | ((array-type float N))
          | (lambda (xs)
          |  ((mapSeq (lambda (x) (* x 2.0f))) xs)))""".stripMargin

  describe("Tokenizer") {
    it("should tokenize correctly") {

      assertResult(Right(Vector(
        LParen, Identifier("+"), IntLiteral(1), Identifier("x"), RParen)))(Token.tokenize("(+ 1 x)"))
      assert(Token.tokenize(code).isInstanceOf[Right[String, Vector[Token]]])
    }

    it("should return error for invalid charctor") {
      assert(Token.tokenize("(& 1 2)").isInstanceOf[Left[String, Vector[Token]]])
    }
  }

  describe("Parser") {
    it("should parse correctly") {
      val tokens = Token.tokenize(code).right.get
      assert(Parser.parse(tokens).isInstanceOf[Right[String, Lift]])
    }

    it("should return error for invalid grammer") {
      assert(safeParse("(+ 1 2)").isInstanceOf[Left[String, Lift]])
      assert(safeParse(code.slice(0, code.length - 1)).isInstanceOf[Left[String, Lift]])
    }
  }

  describe("TypeChecker") {
    it("should check types correctly") {
      import Expression._

      val lift = parse(code)

      assert(TypeChecker.check(lift).isInstanceOf[Either[String, (Expression, Type)]])
      val lambdaOfMapSeq = lift
        .body.asInstanceOf[Lambda]
          .body.asInstanceOf[Apply[Expression]]
          .callee.asInstanceOf[Apply[Expression]] // mapSeq
        .args(0).asInstanceOf[Lambda]
      assertResult(Type.Function(Vector(Type.Float), Type.Float))(lambdaOfMapSeq.ty)
    }
  }

  def safeParse(code: String): Either[String, Lift] = {
    Token.tokenize(code).flatMap(tokens => Parser.parse(tokens))
  }
  def parse(code: String): Lift = safeParse(code).right.get
}
