import org.scalatest.FunSpec
import java.io.File

import token._
import parser._
import ast._
import pass.TypeChecker
import errors._

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

      assert(TypeChecker.check(lift).isInstanceOf[Either[String, Expression]])
      val lambdaOfMapSeq = lift
        .body.asInstanceOf[Lambda]
          .body.asInstanceOf[Apply]
          .callee.asInstanceOf[Apply] // mapSeq
        .args(0).asInstanceOf[Lambda]
      assertResult(Some(Type.Float ->: Type.Float))(lambdaOfMapSeq.ty)
    }
  }

  describe("Integration testing") {
    println((new File("./examples")).getAbsolutePath())
    (new File("./examples")).listFiles(f => f.isFile && f.getName().endsWith(".lisp")).foreach { file =>
      it(s"should compile ${file.getName()} successfully") {
        val ret = MinLift.compile(file.getAbsolutePath())
        lazy val clue = if (ret.isLeft) { s"'${ret.left.get}'" } else { "" }
        assertResult(true, clue)(ret.isRight)
      }
    }
  }

  def safeParse(code: String): Either[LiftError, Lift] = {
    Token.tokenize(code).flatMap(tokens => Parser.parse(tokens))
  }
  def parse(code: String): Lift = safeParse(code).right.get
}
