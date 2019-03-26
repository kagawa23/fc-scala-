package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B):Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElese[B >: A](default: =>B):B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]):Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap1[B](f: A => Option[B]):Option[B] =
    map(f) getOrElese None

  def orElese[B >:A](ob: =>Option[B]):Option[B] = this match {
    case None => ob
    case Some(_) => this
  }
  def filter(f:A=>Boolean):Option[A] = this match {

    case Some(a) if f(a) => this

    case _ => None


  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object  Option {

  def main(args: Array[String]): Unit = {
    println(Some(2).map((x:Int) => x * x))
    println(None.map((x:Int) => x*x ))
    println(Some(2).flatMap((x:Int) => Some(x * x)))
    println(Some(10).filter((x:Int) => x>10))

  }
}
