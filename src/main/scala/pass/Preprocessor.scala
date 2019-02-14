package pass

import ast._
import errors._

class Preprocessor extends ExpressionVisitor[Unit, Either[LiftError, Expression]] {
  override def visit(node: Expression.Apply, a: Unit): ResultType = {
    node.callee match {
      // (o f g h ... x) -> (f (g (h ... x)))
      case Expression.Identifier("o", false) => {
        if (node.args.length < 1) {
          Left(PreprocessError("composition 'o' needs at least one argument"))
        }
        else if (node.args.length == 1) {
          node.args(0).accept(this, ())
        }
        else { // node.args.length >= 2
          val newApply = node.args.dropRight(1).foldRight(node.args.last)((arg, app) => {
            Expression.Apply(arg, List(app))
          })
          newApply.accept(this, ())
        }
      }
      // (((f x) y) z) -> ((f x) y z) -> (f x y z)
      case Expression.Apply(ccallee, cargs) => {
        val newApply = Expression.Apply(ccallee, cargs ++ node.args)
        newApply.accept(this, ())
      }
      case _ => {
        node.callee.accept(this, ()).flatMap(callee => {
          val args = node.args
            .map(_.accept(this, ()))
            .foldRight(Right(List()): Either[LiftError, List[Expression]])((arg, args) => {
              arg match {
                case Right(t) => args.map(t :: _)
                case Left(err) => Left(err)
              }
            })

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

object Preprocessor {
  def normalize(node: Lift) = {
    val norm = new Preprocessor
    node.body.accept(norm, ()).map(body => {
      Lift(node.variables, node.inputTypes, body.asInstanceOf[Expression.Lambda])
    })
  }
}
