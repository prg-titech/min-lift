package pass

import scala.collection._
import ast._

// check and infer types by abstract interpretation
class TypeChecker extends Visitor[Either[String, (Expression, Type)]] {
  type Value = (Expression, Type)
  type TypeCheckerResult = Either[String, Value]
  type LocalEnv = mutable.Map[String, Value]

  val env = mutable.Stack[LocalEnv]()
  env.push(mutable.Map[String, Value]())

  override def visit(node: Lift): TypeCheckerResult = {
    // set default definitions
    // mapSeq :: (A => B) => (A[]n => B[]n)
    env.top += "mapSeq" -> (
      Expression.Undefined(),
      Type.Function(
        Vector(Type.Function(
          Vector(Type.Polymorphic("A")),
          Type.Polymorphic("B"))),
        Type.Function(
          Vector(Type.Array(Type.Polymorphic("A"), Variable("N"))),
          Type.Array(Type.Polymorphic("B"), Variable("N")))))
    env.top += "*" -> (
      Expression.Undefined(),
      Type.Function(
        Vector(Type.Float, Type.Float),
        Type.Float))

    val root = node.body.accept(this)
    root.flatMap(root => {
      Expression.Apply(root._1, node.inputTypes).accept(this)
    })
  }

  override def visit[U](node: Expression.Apply[U]): TypeCheckerResult = {
    node.callee.accept(this).flatMap(callee => {
      // convert Vector[Either[E, X]] to Either[E, Vector[X]]
      val actualArgs = node.args
        .map{
          case arg:Expression => arg.accept(this)
          case arg:Type => Right((Expression.Undefined(), arg))
        }
        .foldLeft(Right(Vector.empty[Value]) : Either[String, Vector[Value]])((args, arg) => {
          arg match {
            case Right(t) => args.map(_ :+ t)
            case Left(err) => Left(err)
          }
        })

      // check types passed to the function
      actualArgs.flatMap(actualArgs => {
        callee._2 match {
          case Type.Function(expectedArgTypes, expectedResultType) => {
            val polymorphicMap = mutable.Map[String, Type]()

            def equalsType(a: Type, b : Type): Boolean = (a, b) match {
              case (Type.Array(aInner, aLength), Type.Array(bInner, bLength)) => {
                aLength.equals(bLength) && satisfiedType(aInner, bInner)
              }
              case (Type.Function(aArgs, aResult), Type.Function(bArgs, bResult)) => {
                aArgs.zip(bArgs).forall { case (aty, bty) => satisfiedType(aty, bty) } && satisfiedType(aResult, bResult)
              }
              case (Type.Float, Type.Float) => true
              case _ => false
            }

            def resolveType(ty: Type): Type = ty match {
              case Type.Array(innerType, length) => Type.Array(resolveType(innerType), length)
              case Type.Polymorphic(x) => {
                polymorphicMap.get(x).getOrElse(Type.Polymorphic(s"Unresolved ${x}"))
              }
              case Type.Unfixed => Type.Unfixed
              case _ => ty
            }

            def satisfiedType(actual: Type, expected: Type): Boolean = {
              (actual, expected) match {
                case (Type.Unfixed, _) => {
                  // check later
                  true
                }
                case (_, Type.Unfixed) => true
                case (_, Type.Polymorphic(x)) => {
                  polymorphicMap.get(x) match {
                    case Some(ty) => equalsType(actual, ty)
                    case None => {
                      polymorphicMap += x -> actual
                      true
                    }
                  }
                }
                case _ => equalsType(actual, expected)
              }
            }

            // resolve unfixed or polymorphic types of the function
            val satisfiedArgTypes = actualArgs
              .zip(expectedArgTypes)
              .forall { case (actual, expected) => satisfiedType(actual._2, expected) }

            if (satisfiedArgTypes) {
              val result = callee._1 match {
                case Expression.Lambda(argIds, body) => {
                  val local = mutable.Map[String, Value]()
                  argIds.zip(actualArgs).foreach { case (id, arg) =>
                    local += id.value -> arg
                  }

                  env.push(local)
                  val result = body.accept(this)
                  env.pop()
                  result
                }
                case Expression.Map(f) => {
                  val Type.Array(innerType, length) = actualArgs(0)._2
                  val result = Expression.Apply(f, Vector(innerType)).accept(this)

                  result.map { case (_, ty) =>
                    f.ty = Type.Function(Vector(innerType), ty)

                    (Expression.Undefined(), Type.Array(ty, length))
                  }
                }
                case Expression.Undefined() => {
                  // built-in function
                  val Expression.Identifier (name, _) = node.callee
                  val result = name match {
                    case "mapSeq" => {
                      val f = actualArgs (0)._1
                      Expression.Map (f)
                    }
                    case "*" => {
                      Expression.Undefined()
                    }
                  }

                  Right ((result, expectedResultType))
                }
              }

              result.flatMap { case (expr, actualResultType) =>
                val satisfiedResultType = satisfiedType(actualResultType, expectedResultType)
                if (satisfiedResultType) {

                  node.callee.ty = Type.Function(
                    expectedArgTypes.map(ty => resolveType(ty)),
                    resolveType(actualResultType)
                  )
                  node.ty = actualResultType

                  Right((expr, actualResultType))
                }
                else {
                  Left(s"result type mismatch. expected ${expectedResultType}, actual: ${actualResultType}")
                }
              }
            }
            else {
              Left(s"argument types mismatch. expected ${expectedArgTypes}, actual: ${actualArgs.map(_._2)}")
            }
          }
          case _ => {
            Left(s"callee(${callee}) is not function")
          }
        }
      })
    })
  }

  override def visit(node: Expression.Lambda): TypeCheckerResult= {
    val ty = Type.Function(node.args.map(_ => Type.Unfixed), Type.Unfixed)
    node.ty = ty
    Right((node, ty))
  }

  override def visit(node: Expression.Map): TypeCheckerResult = ???

  override def visit(node: Expression.Identifier): TypeCheckerResult = {
    for (local <- env) {
      local.get(node.value) match {
        case Some(value) => {
          node.ty = value._2
          return Right(value)
        }
        case None => ()
      }
    }

    Left(s"undefined variable ${node.value}")
  }

  override def visit[U](node: Expression.Const[U]): TypeCheckerResult = node match {
    case Expression.Const(value) => {
      val ty = value match {
        case v:Float => Type.Float
        case v:Double => Type.Double
        case v:Int => Type.Int
      }
      node.ty = ty
      Right((node, ty))
    }
  }

  override def visit(node: Expression.Undefined): TypeCheckerResult = ???
}

object TypeChecker {
  def check(lift: Lift) = (new TypeChecker).visit(lift)
}