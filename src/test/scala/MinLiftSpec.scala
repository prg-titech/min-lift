import org.scalatest.FunSpec
import java.io.File

import token._
import parser._
import ast._
import pass._
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
      assertResult(Right(Vector(Identifier("get1"))))(Token.tokenize("get1"))
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

    it("should return error for invalid grammar") {
      assert(safeParse("(+ 1 2)").isInstanceOf[Left[String, Lift]])
      assert(safeParse(code.slice(0, code.length - 1)).isInstanceOf[Left[String, Lift]])
    }
  }

  describe("TypeChecker") {
    it("should check types correctly") {
      import Expression._

      val lift = parse(code)

      assert(TypeChecker.check(lift).isInstanceOf[Right[String, Expression]])
      val lambdaOfMapSeq = lift
        .body.asInstanceOf[Lambda]
          .body.asInstanceOf[Apply]
          .callee.asInstanceOf[Apply] // mapSeq
        .args(0).asInstanceOf[Lambda]
      assertResult(Type.Float ->: Type.Float)(lambdaOfMapSeq.ty)
    }

    describe("existential type") {
      it("should fail map from filter") {
        val code =
          """
            |(lift
            | (N)
            | ((array-type float N))
            | (lambda (xs)
            |   (mapSeq id (filterSeq (lambda (x) true) xs))))
          """.stripMargin

        val lift = TypeChecker.check(parse(code))

        println(lift)
        assert(lift.isInstanceOf[Left[String, Lift]])
        val Left(err) = lift
        val errMsg = err.toString
        assert(errMsg.contains("unsolvable constraints") && errMsg.contains("Existential"))
      }
      it("should fail zip from two different filter functions") {
        val code =
          """
            |(lift
            | (N)
            | ((array-type float N))
            | (lambda (xs)
            |   (let xs1 (filterSeq (lambda (x) true) xs)
            |     (let xs2 (filterSeq (lambda (x) true) xs)
            |       (pack (zip xs1 xs2))))))
          """.stripMargin

        val lift = TypeChecker.check(parse(code))

        println(lift)
        assert(lift.isInstanceOf[Left[String, Lift]])
        val Left(err) = lift
        assert(err.toString.contains("unsolvable constraints"))
      }
      it("should accept without packing a unpacked array") {
        val code =
          """
            |(lift
            | (N)
            | ((array-type float N))
            | (lambda (xs)
            |   (let ys (filterSeq (lambda (x) true) xs)
            |     ys)))
          """.stripMargin

        val lift = TypeChecker.check(parse(code))

        assert(lift.isInstanceOf[Right[String, Lift]])
      }

      it("should accept packing a unpacked array") {
        val code =
          """
            |(lift
            | (N)
            | ((array-type float N))
            | (lambda (xs)
            |   (let ys (filterSeq (lambda (x) true) xs)
            |     (pack ys))))
          """.stripMargin

        val lift = TypeChecker.check(parse(code))

        // println(lift)
        assert(lift.isInstanceOf[Right[String, Lift]])
      }
      it("should accept zip from two identical filter functions") {
        val code =
          """
            |(lift
            | (N)
            | ((array-type float N))
            | (lambda (xs)
            |  (let ys (filterSeq (lambda (x) true) xs)
            |   (let zs ys
            |    (pack (zip ys zs))))))
          """.stripMargin

        val lift = TypeChecker.check(parse(code))

        // println(lift)
        assert(lift.isInstanceOf[Right[String, Lift]])
      }
      it("should accept unpack with lambda argument") {
        val code =
          """
            |(lift
            | (N)
            | ((array-type float N))
            | (lambda (xs)
            |   ((lambda (x) (let y x (pack (reduceSeq 0.0f + y))))
            |     (filterSeq (lambda (x) (< x 5.0f)) xs))))
          """.stripMargin

        val ast = parse(code);
        val lift = for (
          ast <- Preprocessor.normalize(ast);
          ast <- Preprocessor.kNormalize(ast);
          ast <- TypeChecker.check(ast)
        ) yield { ast }

        assert(lift.isInstanceOf[Right[String, Lift]])
      }
    }

    describe("pack and unpack insertion") {
      it("should accept filter code without pacn and unpack") {
        val code =
          """
            |(lift
            | (N)
            | ((array-type float N))
            | (lambda (xs)
            |   (mapSeq (lambda (x) (* x 2.0f)) (filterSeq (lambda (x) true) xs))))
          """.stripMargin

        val ast = parse(code)
        val kNormAst = Preprocessor.kNormalize(ast).right.get
        println(AstPrinter.print(kNormAst))
        val lift = TypeChecker.check(kNormAst)
        assert(lift.isInstanceOf[Right[String, Lift]])
      }
    }
  }

  describe("Integration testing") {
    (new File("./examples")).listFiles(f => f.isFile && f.getName().endsWith(".lisp")).foreach { file =>
      it(s"should compile ${file.getName()} successfully") {
        val ret = MinLift.compile(file.getAbsolutePath())
        assert(ret.isInstanceOf[Right[Any, String]])
      }
    }
  }

  def safeParse(code: String): Either[LiftError, Lift] = {
    Token.tokenize(code).flatMap(tokens => {
      // println(tokens)
      Parser.parse(tokens)
    })
  }
  def parse(code: String): Lift = {
    val lift = safeParse(code)
    // println(lift)
    lift.right.get
  }
}
