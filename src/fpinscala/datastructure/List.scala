package fpinscala.datastructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints:List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def tail(ints:List[Int]):List[Int] = ints match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }

  def setHead(ints:List[Int],value:Int):List[Int] =  ints match {
    case Cons(x,xs) => Cons(value,xs)
    case Nil => Cons(value, Nil)
  }

  def drop1(ints:List[Int],n:Int):List[Int] = ints match {
    case Cons(x,xs) => if(n>0) {
      println(n)
      println(xs)
      drop(xs,n-1)
    } else xs
    case Nil => Nil
  }

  def drop(ints:List[Int],n:Int):List[Int] =
    if(n <= 0)ints
    else ints match {
      case Nil => Nil
      case Cons(_,t) => {
        println(n)
        println(t)
        drop(t,n-1)
      }
    }


  def main(args: Array[String]): Unit = {
    val as = List (1,2,3,4,5,6)

//    println(sum(as))
//    // 3.1
//
//    //3.2
//    println(tail(as))
//
//    println(setHead(as,5))


//    println(drop(as,3))


    println(drop1(as,3))
//
//    val x = List(1,2,3,4,5) match {
//      case Cons(x,Cons(2, Cons(4,_))) => x
//      case Nil => 42
//      case Cons(x,Cons(y,Cons(3,Cons(4,_)))) => x+y
//      case Cons(h,t) => h + sum(t)
//      case _ => 101
//    }
//    println(x)

//    println(tail(as))

  }

}
