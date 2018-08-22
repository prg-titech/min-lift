import java.io.PrintWriter

import scala.io.Source
import scala.collection.immutable.Vector
import token._
import parser._
import ast._
import pass._

object MinLift {
  def main(args: Array[String]): Unit = {
    val srcPath = args(0)
    val destPath = srcPath.substring(0, srcPath.lastIndexOf('.')) + ".cl"

    val source = Source.fromFile(srcPath).getLines().mkString("\n")
//    val tokens = tokenize("(+ 1 2)")
    val res = for (
       tokens <- Token.tokenize(source);
       ast <- Parser.parse(tokens);
       // typedAst <- TypeChecker.check(ast);
       typedAst <- Right(ast);
       norm <- Right(Normalizer.normalize(typedAst));
       _ <- Right(println(AstPrinter.print(norm)));
       typedNorm <- TypeChecker.check(norm);
       _ <- MemoryAllocator.inferAddressSpace(typedNorm);
       code <- Right(CodeGenerator.generate(typedNorm))
    ) yield {
      println(tokens)
      println("success checking type and allocating memory!")
      println(AstPrinter.print(typedAst))
      println(AstPrinter.print(norm))
      println(AstPrinter.print(typedNorm))

      println(code)
      val dest = new PrintWriter(destPath)
      dest.write(code)
      dest.close()
      println("output code\n")
    }

    if (res.isLeft) {
      println(res.left.get)
    }
  }
}
