package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
  //25. Ok
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }
  
  //26. Ok
  def maximum[A](tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
    
  }
  
  //27.Ok
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left,right) => 1 + (depth(left) max depth(right))
  }
 
  //28. Ok
  def map[A,B](tree:Tree[A])(f:(A) => B):Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
  
  //29.  Ok
  def fold[A,B](tree: Tree[A])(f: A =>B)(g: (B,B)=>B):B = tree match{
    case Leaf(a) =>f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    
  }
  
  def sizeViaFold(tree: Tree[Int]): Int = 
    fold(tree)(_ => 1)(1 + _ + _)

  
  def maximumViaFold(tree: Tree[Int]): Int = 
    fold(tree)(v => v)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
  fold(tree)(_=>0)((l,r) => 1 + (l max r))
  
  def mapViaFold[A,B](tree: Tree[A])(f: (A) => B): Tree[B] = 
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}