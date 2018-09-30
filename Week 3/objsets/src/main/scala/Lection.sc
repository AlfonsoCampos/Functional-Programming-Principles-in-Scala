/* Class Hierarchies */
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "."
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4

abstract class Base {
  def foo = 1
  def bar: Int
}

class Sub extends Base {
  override def foo = 2
  def bar = 3
}

/* How Classes Are Organized */
trait Planar {
  def height: Int
  def width: Int
  def surface = height * width
}

def error(msg: String) = throw new Error(msg)

val x = null
val y: String = x
//val z: Int = null

if (true) 1 else false

/* Polymorphism */
//trait IntList
//class Cons(val head: Int, val tail: IntList) extends IntList
//class Nil extends IntList

trait List[T]{
  def isEmpty: Boolean
  def head: T
  def tail : List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty: Boolean = false
}

import java.util.NoSuchElementException
class Nil[T] extends List[T]{
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
singleton[Int](1)
singleton[Boolean](true)

singleton(1)
singleton(true)

def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n-1, xs.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(2, list)
nth(-1, list)

