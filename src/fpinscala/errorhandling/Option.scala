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
  def mean(xs:Seq[Double]):Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  def variance(xs:Seq[Double]):Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(xx => math.pow(xx -m ,2))))
  def map2[A,B,C](a:Option[A],b:Option[B])(f:(A,B)=>C):Option[C] =
    a.flatMap(aa => b.map(bb => f(aa,bb)))


  def map2_1[A,B,C](a:Option[A],b:Option[B])(f:(A,B)=>C):Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)

  def sequence[A](a:List[Option[A]]):Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(h,sequence(t))(_::_)
  }

  def sequence1[A](a:List[Option[A]]):Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((aa:Option[A],z:Option[List[A]])=>  map2(aa,z)(_::_))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_::_))


  def main(args: Array[String]): Unit = {
    println(Some(2).map((x:Int) => x * x))
    println(None.map((x:Int) => x*x ))
    println(Some(2).flatMap((x:Int) => Some(x * x)))
    println(Some(10).filter((x:Int) => x>10))
//    println(variance(Seq()))
    println(sequence(List(Some(1),Some(3),Some(5),Some(7))))
    println(sequence1(List(Some(1),Some(3),Some(5),Some(7))))
    println(traverse(List(1,3,5,6))(Some(_)))

  }
}
