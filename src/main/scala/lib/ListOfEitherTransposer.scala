package lib

object ListOfEitherTransposer {
  def transpose[E, X](listOfEither: List[Either[E, X]]): Either[E, List[X]] = {
    listOfEither
      .foldRight(Right(List()): Either[E, List[X]])((arg, args) =>
        arg match {
          case Right(t) => args.map(t :: _)
          case Left(err) => Left(err)
        }
      )
  }
}
