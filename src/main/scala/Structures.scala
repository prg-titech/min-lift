package structures

import scala.collection._

trait Environment[T] {
  def lookup(id: String): Option[T]
  def append(id: String, value: T): Environment[T]

  def pushEnv(mapper: Map[String, T]) = ConsEnvironment(mapper, this)
}
case class ConsEnvironment[T](val mapper: Map[String, T], val next: Environment[T]) extends Environment[T] {
  def append(id: String, value: T) = {
    ConsEnvironment(mapper + (id -> value), next)
  }

  def lookup(id: String): Option[T] = {
    mapper.get(id) match {
      case ts@Some(_) => ts
      case None => next.lookup(id)
    }
  }
}
case class EmptyEnvironment[T]() extends Environment[T] {
  def append(id: String, value: T) = this
  def lookup(id: String): Option[T] = None
}

