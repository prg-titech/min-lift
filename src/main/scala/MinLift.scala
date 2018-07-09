import scala.io.Source
import scala.collection.immutable.Vector

import token._
import parser._
import ast._
import pass._

object MinLift {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(args(0)).getLines().mkString("\n")
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
//      println("output code\n")
//      println((new CodeGenerator).visit(ast))
    }

    if (res.isLeft) {
      println(res.left.get)
    }
  }
}
