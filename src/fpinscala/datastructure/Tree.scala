package fpinscala.datastructure

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]):Int = t match {
    case Branch(a,b) => size(a) + size(b) + 1
    case Leaf(_) => 1
  }

  def depth[A](t: Tree[A]):Int = t match {
    case Branch(a,b) => (depth(a) max depth(b) ) + 1
    case Leaf(_) => 0
  }

  def maximum(t: Tree[Int]):Int = t match {
    case Branch(a,b) => (maximum(a) max maximum(b) )
    case Leaf(a) => a
  }

  def map[A,B](t: Tree[A])(f:A=>B):Tree[B] = t match {
    case Branch(a,b) => Branch(map(a)(f),map(b)(f))
    case Leaf(a) => Leaf(f(a))
  }

//  def fold(t:Tree[A])()
}

object Test extends App {
  val a = Branch(Leaf("1"), Branch(Branch(Leaf("2"),Leaf("3")),Leaf("4")))
  val b = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(5)),Leaf(1)))

println(Tree.size(a))

  println(Tree.depth(a))


  println(Tree.maximum(b))

  println(Tree.map[Int,Int](b)(_+1))
}

