import java.io.PrintWriter

import scala.io.Source
import scala.collection.immutable.Vector
import token._
import parser._
import ast._
import pass._

object MinLift {
  def compile(srcPath: String): Either[Any, String] = {
    val source = Source.fromFile(srcPath).getLines().mkString("\n")
    for (
       tokens <- Token.tokenize(source);
       ast <- Parser.parse(tokens);
       ast <- Right(ast);
//       norm <- Right(Normalizer.normalize(typedAst));
       ast <- Right(ast);
       _ <- Right(println(AstPrinter.print(ast)));
       ast <- Right(Normalizer.normalize(ast));
       ast <- TypeChecker.check(ast);
       _ <- Right(ArrayAccessSolver.solve(ast));
       _ <- Right(println("=== typedNorm ==="));
       _ <- Right(println(AstPrinter.print(ast)));
       _ <- MemoryAllocator.inferAddressSpace(ast);
       _ <- Right(println("=== final ==="));
       _ <- Right(println(AstPrinter.print(ast)));
       code <- Right(CodeGenerator.generate(ast))
    ) yield {
      println("success checking type and allocating memory!")

      val destPath = srcPath.substring(0, srcPath.lastIndexOf('.')) + ".cl"
      val dest = new PrintWriter(destPath)
      dest.write(code)
      dest.close()
      println("output code\n")
      println(code)

      code
    }
  }

  def main(args: Array[String]): Unit = {
    val srcPath = args(0)

    compile(srcPath) match {
      case Right(code) => {}
      case Left(err) => {
        println(err)
      }
    }
  }
}
