package lift

import lib.{UniqueIdGenerator, Environment, EmptyEnvironment}
import ast._
import ast.Type._
import pass._

object BuiltInFunctions {
  def generateTypeVar(idGen: UniqueIdGenerator) = Type.TypeVar(s"t${idGen.generateInt()}")

  def getFuncs(idGen: UniqueIdGenerator): Environment[TypeScheme] = {
    val a = generateTypeVar(idGen)
    val b = generateTypeVar(idGen)
    val c = generateTypeVar(idGen)

    EmptyEnvironment().pushEnv(Map(
      ("mapSeq" -> TypeScheme(List(a, b, c), (a ->: b) ->: Array(a, c) ->: Array(b, c))),
      ("mapGlb" -> TypeScheme(List(a, b, c), (a ->: b) ->: Array(a, c) ->: Array(b, c))),
      ("mapWrg" -> TypeScheme(List(a, b, c), (a ->: b) ->: Array(a, c) ->: Array(b, c))),
      ("mapLcl" -> TypeScheme(List(a, b, c), (a ->: b) ->: Array(a, c) ->: Array(b, c))),
      ("reduceSeq" -> TypeScheme(List(a, b, c), b ->: (b ->: a ->: b) ->: Array(a, c) ->: Array(b, SizeConst(1)))),
      ("filterSeq" -> TypeScheme(List(a, b, c), (a ->: Boolean) ->: Array(a, b) ->: Existential(Array(a, c)))),
      ("filterGlb" -> TypeScheme(List(a, b, c), (a ->: Boolean) ->: Array(a, b) ->: Existential(Array(a, c)))),
      ("split" -> TypeScheme(List(a, b, c), a ->: Array(b, c) ->: Array(Array(b, a), SizeDivision(c, a)))),
      ("join" -> TypeScheme(List(a, b, c), Array(Array(a, b), c) ->: Array(a, SizeMultiply(b, c)))),
      ("zip" -> TypeScheme(List(a, b, c), Array(a, c) ->: Array(b, c) ->: Array(Tuple2(a, b), c))),
      ("get1" -> TypeScheme(List(a, b), Tuple2(a, b) ->: a)),
      ("get2" -> TypeScheme(List(a, b), Tuple2(a, b) ->: b)),
      ("id" -> TypeScheme(List(a), a ->: a)),
      ("toGlobal" -> TypeScheme(List(a, b), (a ->: b) ->: (a ->: b))),
      ("toLocal" -> TypeScheme(List(a, b), (a ->: b) ->: (a ->: b))),
      ("toPrivate" -> TypeScheme(List(a, b), (a ->: b) ->: (a ->: b))),

      // Float operators
      ("*" -> TypeScheme(List(), Float ->: Float ->: Float)),
      ("+" -> TypeScheme(List(), Float ->: Float ->: Float)),
      ("<" -> TypeScheme(List(), Float ->: Float ->: Boolean)),
      (">" -> TypeScheme(List(), Float ->: Float ->: Boolean)),

      // Int operators
      ("*i" -> TypeScheme(List(), Int ->: Int ->: Int)),
      ("=i" -> TypeScheme(List(), Int ->: Int ->: Boolean)),
      ("mod" -> TypeScheme(List(), Int ->: Int ->: Int)),

      // Boolean operators
      ("or" -> TypeScheme(List(), Boolean ->: Boolean ->: Boolean)),
    ))
  }
}
