package fpinscala.laziness


sealed  trait Stream[+A] {
  def foldRight[B](z: =>B)(f:(A, =>B) => B):B =
    this match {
      case Cons(h,t) => f(h(),t().foldRight(z)(f))
      case _ => z
    }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }


  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toListTailRecursive:List[A] = {
    def go(s:Stream[A],l:List[A]):List[A] = s match {
      case Cons(h, t) => go(t(),h()::l)
      case _ => l
    }
    go(this,List()).reverse
  }
  def drop(n:Int):Stream[A] = this match {
    case Cons(_, t) if(n > 0) => t().drop(n-1)
    case _ => this
  }

  def take(n:Int):Stream[A] = this match {
    case Cons(h,t) if(n>0) => Stream.cons(h(),t().take(n-1))
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match{
    case Cons(h,t) if(p(h())) => Stream.cons(h(),t().takeWhile(p))
    case _ => Empty

  }

  def exists(p:A=> Boolean):Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) && t().exists(p)
    case _ => true
  }

  def takeWhile1(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h,t)=>if(p(h)) Stream.cons(h,t) else Empty)

  def headOption1: Option[A] = foldRight(None:Option[A])((h,_)=>Some(h))

  def map[B](f:A=>B):Stream[B] = foldRight(Stream.empty[B])((curr,accu)=> Stream.cons(f(curr),accu))

  def filter(f:A=>Boolean):Stream[A] = foldRight(Stream.empty[A])((curr,accu)=> if(f(curr)) Stream.cons(curr,accu) else accu)

  def append[B>:A](s: =>Stream[B]):Stream[B] = foldRight(s)((curr,accu) => Stream.cons(curr,accu))

  def flatMap[B](f:A=>Stream[B]):Stream[B] = foldRight(Stream.empty[B])((curr,accu) =>f(curr) append(accu))

  def map1[B](f:A=>B):Stream[B] = Stream.unfold(this)({
    case Cons(h,t) => Some((f(h()),t()))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this, s2))({
    case (Cons(h,t),Cons(h1,t1)) => Some()
  })
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h:()=>A, t:() => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A,tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=> head, ()=> tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if(as.isEmpty) empty else cons(as.head, apply(as.tail:_*))

  def constant[A](a: A): Stream[A] = Stream.cons(a,constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n,from(n+1))

  def fibs():Stream[Int] = {
    def go(n1:Int,n2:Int):Stream[Int] = {
      Stream.cons(n1,go(n2,n1+n2))
    }

    go(0,1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((cur,accu)) => Stream.cons(cur, unfold(accu)(f))
    case None => Stream.empty
  }

  def fibs1(): Stream[Int] = unfold((0,1))( accu => accu match {
    case (f0,f1) => Some(f0,(f1, f0+f1))
  } )

  def from1(n:Int): Stream[Int] = unfold(n)(n=> Some((n,n+1)))

  def constant(n:Int): Stream[Int] = unfold(n)(n=> Some((n,n)))

  def main(args: Array[String]): Unit = {
//    println(Stream(1,2,3).toList)
//    println(Stream(1,2,3).toListTailRecursive)
    println(Stream(1,2,3).drop(12).toList)
    println(Stream(1,2,3).take(8).toList)
    println(Stream(1,2,3).takeWhile(x=> x<2).toList)
    println(Stream(1,2,3).takeWhile1(x=> x<2).toList)

    println(Stream(1,2,3).exists(_>1))
    println(Stream(1,2,3).exists(_>0))

    println(Stream(1,2,3).headOption1)

    println(Stream(1,2,3).map(_+1).toList)

    println(Stream(1,2,3).filter(_>1).toList)

    println(Stream(1,2,3).append(Stream(3,4)).toList)

    println(constant(2).take(20).toList)

    println(from(2).take(20).toList)

    println(fibs().take(20).toList)

    println(fibs1().take(10).toList)


    println(from1(2).take(20).toList)

    println(Stream(1,2,3).map1(_+1).toList)


  }
}
