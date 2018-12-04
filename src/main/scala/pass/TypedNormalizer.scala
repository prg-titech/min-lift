package pass

import ast._

class TypedNormalizer extends ExpressionVisitor[Unit, Expression] {
  var varCount = 0
  def mkTemp: String = {
    varCount += 1
    s"temp$varCount"
  }

  // (f: (A => B => C) x): (B => C) -> (lambda (temp1) (f: (A => B => C) x temp1): (B => C)
  override def visit(node: Expression.Apply, a: Unit): ResultType = {
    val resultType = node.args.foldRight(node.callee.ty) { (_, ty) =>
      // ty must be arrow
      val Type.Arrow(arg, res) = ty
      res
    }
    val applyArgs = node.args.map(_.accept(this, ()))
    resultType match {
      case arr@Type.Arrow(_, _) => {
        val lambdaArgs = arr.args.map { ty =>
          val id = Expression.Identifier(mkTemp, true)
          id.ty = ty
          id
        }
        val body = node.copy(args = applyArgs ++ lambdaArgs.map { id =>
          val copiedId = id.copy(isParam = false)
          copiedId.ty = id.ty
          copiedId
        })
        body.ty = node.ty
        val lambda = Expression.Lambda(lambdaArgs, body)
        lambda.ty = resultType
        lambda
      }
      case _ => {
        val copied = node.copy(args = applyArgs)
        copied.ty = node.ty
        copied
      }
    }
  }

  override def visit(node: Expression.Lambda, a: Unit): ResultType = {
    val lambda = node.copy(body = node.body.accept(this, ()))
    lambda.ty = node.ty
    lambda
  }

  override def visit(node: Expression.Identifier, a: Unit): ResultType = {
    node
  }

  override def visit[C](node: Expression.Const[C], a: Unit): ResultType = {
    node
  }

  override def visit(node: Expression.Size, a: Unit): ResultType = {
    node
  }
}

object TypedNormalizer {
  def normalize(node: Lift) = {
    val norm = new TypedNormalizer
    node.copy(body = node.body.accept(norm, ()).asInstanceOf[Expression.Lambda])
  }
}
