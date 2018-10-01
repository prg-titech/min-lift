package pass

import ast._

// Allocation memory to expression by the algorithm based on "by P79 Algorithm 1"
// in "Lift: A Functional Data-Parallel IR for High-Performance GPU Code Generation"
//    Michel Steuwer, Toomas Remmelg, Christophe Dubach
object MemoryAllocator  {
  sealed abstract class AddressSpace(clModifier: String) {
    def toCL: String = clModifier
  }
  case object PrivateMemory extends AddressSpace("private")
  case object LocalMemory extends AddressSpace("local")
  case object GlobalMemory extends AddressSpace("global")

  def inferAddressSpace(lift: Lift): Either[Unit, Unit] = {
    val Expression.Lambda(params, body) = lift.body
    lift.inputTypes.zip(params).foreach { case (inputType, param) =>
      if (inputType.isInstanceOf[Type.Scalar]) {
        param.addressSpace = Some(PrivateMemory)
      }
      else {
        param.addressSpace = Some(GlobalMemory)
      }
    }
    inferAddressSpaceExpr(body, None)

    Right(())
  }

  def inferAddressSpaceExpr(expr: Expression, writeTo: Option[AddressSpace]): Unit = {
    import Expression._

    // Is it needed?
    expr.addressSpace = writeTo

    expr match {
      case Const(_) => expr.addressSpace = Some(PrivateMemory)
      case Identifier(_, true) /* as Param */ => assert(expr.addressSpace != None)
      case Apply(f, argsAny) => {
        val args = argsAny.map { case arg:Expression => arg }

        args.foreach(arg => inferAddressSpaceExpr(arg, writeTo))

        f match {
          // TODO implement following expressions
          // case UserFun
          // case toGlobal
          // case Reduce
          // case Iterate
          case l@Lambda(_, _) => inferAddressSpaceApply(l, args, writeTo)
          case Identifier("mapSeq", false) => inferAddressSpaceExpr(args(0), writeTo)
          case Identifier("toLocal", false) => inferAddressSpaceExpr(args(0), Some(LocalMemory))
          case Identifier("toPrivate", false) => inferAddressSpaceExpr(args(0), Some(PrivateMemory))
          case Apply(_, _) => inferAddressSpaceExpr(f, writeTo)
          case _ => expr.addressSpace = args.lift(0).flatMap(_.addressSpace) // not args.addressSpace
        }
      }
      case Lambda(_, body) => inferAddressSpaceExpr(body, writeTo)
      case _ => {}
    }
  }

  def inferAddressSpaceApply(lambda: Expression.Lambda, args: List[Expression], writeTo: Option[AddressSpace]): Unit = {
    lambda.args.zip(args).foreach { case (p, a) => p.addressSpace = a.addressSpace }
    inferAddressSpaceExpr(lambda.body, writeTo)
  }
}
