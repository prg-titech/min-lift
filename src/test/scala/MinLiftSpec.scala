import org.scalatest.FunSpec

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
      import Token._

      assertResult(Right(Vector(
        LParen, Identifier("+"), IntLiteral(1), Identifier("x"), RParen)))(MinLift.tokenize("(+ 1 x)"))
      assert(MinLift.tokenize(code).isInstanceOf[Right[String, Vector[Token]]])
    }

    it("should return error for invalid charctor") {
      assert(MinLift.tokenize("(& 1 2)").isInstanceOf[Left[String, Vector[Token]]])
    }
  }

  describe("Parser") {
    it("should parse correctly") {
      val tokens = MinLift.tokenize(code).right.get
      assert(MinLift.parse(tokens).isInstanceOf[Right[String, Ast.Lift]])
    }

    it("should return error for invalid grammer") {
      assert(safeParse("(+ 1 2)").isInstanceOf[Left[String, Ast.Lift]])
      assert(safeParse(code.slice(0, code.length - 1)).isInstanceOf[Left[String, Ast.Lift]])
    }
  }

  describe("TypeChecker") {
    it("should check types correctly") {
      import Ast.Expression._
      import Ast.Type._

      val lift = parse(code)
      val checker = new TypeChecker

      assert(checker.visit(lift).isInstanceOf[Either[String, (Ast.Expression, Ast.Type)]])
      val lambdaOfMapSeq = lift
        .body.asInstanceOf[Lambda]
          .body.asInstanceOf[Apply[Ast.Expression]]
          .callee.asInstanceOf[Apply[Ast.Expression]] // mapSeq
        .args(0).asInstanceOf[Lambda]
      assertResult(Function(Vector(Float), Float))(lambdaOfMapSeq.ty)
    }
  }

  def safeParse(code: String): Either[String, Ast.Lift] = {
    MinLift.tokenize(code).flatMap(tokens => MinLift.parse(tokens))
  }
  def parse(code: String): Ast.Lift = safeParse(code).right.get
}
