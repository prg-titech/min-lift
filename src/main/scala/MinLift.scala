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
       typedAst <- Right(ast);
       norm <- Right(Normalizer.normalize(typedAst));
       _ <- Right(println(AstPrinter.print(norm)));
       typedNorm <- TypeChecker.check(norm);
       _ <- MemoryAllocator.inferAddressSpace(typedNorm);
       _ <- Right(println("=== typedNorm ==="));
       _ <- Right(println(AstPrinter.print(typedNorm)));
       code <- Right(CodeGenerator.generate(typedNorm))
    ) yield {
      code
    }
  }

  def main(args: Array[String]): Unit = {
    val srcPath = args(0)

    compile(srcPath) match {
      case Right(code) => {
        println("success checking type and allocating memory!")

        val destPath = srcPath.substring(0, srcPath.lastIndexOf('.')) + ".cl"
        val dest = new PrintWriter(destPath)
        dest.write(code)
        dest.close()
        println("output code\n")
      }
      case Left(err) => {
        println(err)
      }
    }
  }
}
