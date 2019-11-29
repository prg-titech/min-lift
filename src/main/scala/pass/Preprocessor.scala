package pass

import ast._
import errors._
import lib._

class Preprocessor extends ExpressionVisitor[Unit, Either[LiftError, Expression]] {
  val compVarGen = new UniqueIdGenerator("comp")

  override def visit(node: Expression.Apply, a: Unit): ResultType = {
    node.callee match {
      // (o f g h ...) -> λx. (f (g (h ... x)))
      case Expression.Identifier("o", false) => {
        if (node.args.length < 1) {
          Left(PreprocessError("composition 'o' needs at least one argument"))
        }
        else if (node.args.length == 1) {
          node.args(0).accept(this, ())
        }
        else { // node.args.length >= 2
          val x = compVarGen.generateString()
          val newApply = node.args.foldRight(Expression.Identifier(x, false): Expression)((arg, app) => {
            Expression.Apply(arg, List(app))
          })
          val newLambda = Expression.Lambda(
            List(Expression.Identifier(x, true)), newApply)
          newLambda.accept(this, ())
        }
      }
      // (((f x) y) z) -> ((f x) y z) -> (f x y z)
      /*case Expression.Apply(ccallee, cargs) => {
        val newApply = Expression.Apply(ccallee, cargs ++ node.args)
        newApply.accept(this, ())
      }
       */
      case _ => {
        node.callee.accept(this, ()).flatMap(callee => {
          val args = ListOfEitherTransposer.transpose(node.args.map(_.accept(this, ())))

          args.map(args => {
            Expression.Apply(callee, args)
          })
        })
      }
    }
  }

  override def visit(node: Expression.Lambda, a: Unit): ResultType = {
    node.body.accept(this, ()).map(body => {
      Expression.Lambda(node.args, body)
    })
  }

  override def visit(node: Expression.Let, a: Unit): ResultType = {
    node.value.accept(this, ()).flatMap(value => {
      node.body.accept(this, ()).map(body => {
        Expression.Let(node.id, value, body, node.unpack)
      })
    })
  }

  override def visit(node: Expression.Pack, a: Unit): ResultType = {
    node.value.accept(this, ()).map(value =>
      Expression.Pack(value)
    )
  }

  override def visit(node: Expression.Identifier, a: Unit): ResultType = {
    Right(node)
  }

  override def visit[C](node: Expression.Const[C], a: Unit): ResultType = {
    Right(node)
  }

  override def visit(node: Expression.Size, a: Unit): ResultType = {
    Right(node)
  }
}

class KNormalizer extends ExpressionVisitor[Unit, Either[LiftError, Expression]] {
  val idGen = new UniqueIdGenerator

  override def visit(node: Expression.Apply, a: Unit): ResultType = {
    node.callee.accept(this, ()).flatMap(callee => {
      val args = ListOfEitherTransposer.transpose(node.args.map(_.accept(this, ())))
      args.map(args => {
        val ids = args.map(arg => {
          if (arg.isInstanceOf[Expression.Apply]) {
            val id = Expression.Identifier(s"temp${idGen.generateInt()}", false)
            (id, Some((id, arg)))
          }
          else {
            (arg, None)
          }
        })
        // TODO: Should I bind callee too?
        val body = Expression.Apply(callee, ids.map(_._1))

        ids.foldRight(body : Expression)((arg, body) => {
          arg._2
            .map(idValue => Expression.Let(idValue._1, idValue._2, body, false))
            .getOrElse(body)
        })
      })
    })
  }

  override def visit(node: Expression.Lambda, a: Unit): ResultType = {
    node.body.accept(this, ()).map(body => {
      Expression.Lambda(node.args, body)
    })
  }

  override def visit(node: Expression.Let, a: Unit): ResultType = {
    node.value.accept(this, ()).flatMap(value => {
      node.body.accept(this, ()).map(body => {
        Expression.Let(node.id, value, body, node.unpack)
      })
    })
  }

  override def visit(node: Expression.Pack, a: Unit): ResultType = {
    node.value.accept(this, ()).map(value =>
      Expression.Pack(value)
    )
  }

  override def visit(node: Expression.Identifier, a: Unit): ResultType = {
    Right(node)
  }

  override def visit[C](node: Expression.Const[C], a: Unit): ResultType = {
    Right(node)
  }

  override def visit(node: Expression.Size, a: Unit): ResultType = {
    Right(node)
  }
}

object Preprocessor {
  def kNormalize(node: Lift) = {
    val norm = new KNormalizer
    node.body.accept(norm, ()).map(body => {
      Lift(node.variables, node.inputTypes, body.asInstanceOf[Expression.Lambda])
    })
  }

  def normalize(node: Lift) = {
    val norm = new Preprocessor
    node.body.accept(norm, ()).map(body => {
      Lift(node.variables, node.inputTypes, body.asInstanceOf[Expression.Lambda])
    })
  }
}
