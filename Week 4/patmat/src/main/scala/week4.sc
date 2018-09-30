// Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predeccesor: Nat
  def succesor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat{
  def isZero = true
  def predeccesor = throw new Error("0.predeccesor")
  def succesor = new Succ(this)
  def +(that: Nat) = that
  def -(that: Nat)  = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat{
  def isZero = false
  def predeccesor = n
  def succesor = new Succ(this)
  def +(that: Nat) = new Succ(n + that)
  def -(that: Nat) = if (that.isZero) this else n - that.predeccesor
}

val a = Zero.succesor
val b = Zero.succesor.predeccesor

a.isZero
b.isZero

(a + b).isZero
(a + b).predeccesor.isZero

// Functions as Objects
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  // List(1, 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))

  // List() = List.apply()
  def apply[T]() = new Nil
}

// Variance
trait List2[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List2[T]
  //def preprend(elem:T): List2[T] = new Cons2(elem, this)
  def preprend [U >: T] (elem: U): List2[U] = new Cons2(elem, this)
}

class Cons2[T](val head: T, val tail: List2[T]) extends List2[T] {
  def isEmpty = false
}

object Nil2 extends List2[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

val x: List2[String] = Nil2

// Requires IntSet here...!
//def f(xs: List[NonEmpty], x: Empty) = xs prepend x

// Pattern Matching
trait Expr{
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
}

show(Sum(Number(1), Number(2)))



