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

  def dropWhile(ints:List[Int],f:(Int)=>Boolean):List[Int] = ints match {
    case Cons(x,xs) => if(f(x)) dropWhile(xs,f) else xs
    case Nil => Nil
  }

  def init(ints:List[Int]):List[Int] = ints match {
    case Cons(_,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
    case Nil => Nil
  }

  def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B):B = as match{
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }

  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B):B = as match{
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }




  def sum1(ns:List[Int]):Int = foldRight(ns,0)(_+_)
  def multiply(ns:List[Int]):Int = foldRight(ns,1)(_*_)
//  def init2(ns:List[Int]):List[Int] = foldRight(ns,Nil)(Cons(_,_))
  def length(ns:List[Int]):Int = foldRight(ns,0)((_,z)=>z+1)

  def sum2(ns:List[Int]):Int = foldLeft(ns,0)(_+_)
  def multiply2(ns:List[Int]):Int = foldLeft(ns,1)(_*_)
  def append[A](ns:List[A],ls:List[A]):List[A] =
    ns match {
      case Nil => ls
      case Cons(x,xs) => Cons(x,append(xs,ls))
    }
//  def append1(ns:List[Int]):List[Int] = foldRight(ns,Nil:List[Int])((x,z)=>Cons(x+1,z))

  def concat[A] (as: List[List[A]]) :List[A] = foldLeft(as, List[A]())((a, b) => append(a,b))


  def convertListOfDoubleToString(ns:List[Double]):List[String] = foldRight(ns,Nil:List[String])((cur,accu)=>Cons(cur.toString,accu))

  def map[A,B](as:List[A])(f:A =>B):List[B] = foldRight(as,Nil:List[B])((c,a)=>Cons(f(c),a))

  def filter[A](as:List[A])(f:A=>Boolean):List[A] = foldRight(as,Nil:List[A])((c,a) => if(f(c)) Cons(c,a) else a)

  def flatmap[A,B](as:List[A])(f:A =>List[B]):List[B] = foldRight(as,Nil:List[B])((c,a)=>append(f(c),a))

  def filter1[A](as:List[A])(f:A=>Boolean):List[A] = flatmap(as)(a=>if(f(a)) List(a)else Nil)

  def addTwoLists(a:List[Int])(b:List[Int]):List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1,t),Cons(b1,y)) => Cons(b1 + b1, addTwoLists(t)(y))
  }
//
//  def add (l: List[Int]) (r: List[Int]): List[Int] = (l,r) match {
//    case (Nil, _) => Nil
//    case (_, Nil) => Nil
//    case (Cons(h,t), Cons(x,y)) => Cons(h+x, add(t)(y))
//
//  }

  def main(args: Array[String]): Unit = {
    val as = List (1,2,3,4,5,6)

    val as1 = List (0.1,0.2,0.3,0.4,0.5,0.6)

//    println(sum(as))
//    // 3.1
//
//    //3.2
//    println(tail(as))
//
//    println(setHead(as,5))


//    println(drop(as,3))


//    println(drop1(as,3))

//    println(dropWhile(as,x=>x<2))
//    println(init(as))
//    println(sum1(as))
//    println(multiply(as))
//    println(length(as))


//    println(sum2(as))
//    println(multiply2(as))
//    println(append(as,as))
//    println(append1(as))
//    println(convertListOfDoubleToString(as1))
//    println(map(as1)(_+1))
//    println(filter(as)(_>2))
    println(addTwoLists(as)(as))
//    println(append(as,as))
//  println(flatmap(as)((a) =>List(a,a)))
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
