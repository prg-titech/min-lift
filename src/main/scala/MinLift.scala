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
       _ <- TypeChecker.check(ast);
       norm <- Right(Normalizer.normalize(ast));
       _ <- MemoryAllocator.inferAddressSpace(norm);
       code <- Right(CodeGenerator.generate(norm))
    ) yield {
      println(tokens)
      pprint.pprintln(ast)
      println("success checking type and allocating memory!")
      println(AstPrinter.print(ast))
      println(AstPrinter.print(norm))
      println(code)
      val dest = new PrintWriter(destPath)
      dest.write(code)
      dest.close()
//      println("output code\n")
//      println((new CodeGenerator).visit(ast))
    }

    if (res.isLeft) {
      println(res.left.get)
    }
  }
}
