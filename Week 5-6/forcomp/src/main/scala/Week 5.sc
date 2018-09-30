// More functions on Lists
def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)

removeAt(1, 1::2::3::Nil)

// Pairs & Tuples
def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y:: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val nums = List(2, -4, 5, 7, 1)
msort(nums)

// Implicit Parameters
def msort2[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y:: ys1) =>
        if (lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val (fst, snd) = xs splitAt n
    merge(msort2(fst)(lt), msort2(snd)(lt))
  }
}

msort2(nums)((x: Int, y:Int) => x < y)

val fruits = "apple" :: "pineapple" :: "orange" :: "banana" :: Nil
msort2(fruits)((x: String, y: String) => x.compareTo(y) < 0)

msort2(nums)((x, y) => x < y)

import math.Ordering
def msort3[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y:: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val (fst, snd) = xs splitAt n
    merge(msort3(fst)(ord), msort3(snd)(ord))
  }
}

msort3(nums)(Ordering.Int)
msort3(fruits)(Ordering.String)

def msort4[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y:: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

    val (fst, snd) = xs splitAt n
    merge(msort4(fst), msort4(snd))
  }
}

msort4(nums)
msort4(fruits)

// Higher-Order List Functions
def squareList(xs: List[Int]): List[Int] =
xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] =
xs map (x => x * x)

val l = 1 :: 2 :: 3 :: Nil
squareList(l)
squareList2(l)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (first, rest) = xs span ( y => y == x)
    first :: pack(rest)
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)

def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (y => (y.head, y.length))

encode(List("a", "a", "a", "b", "c", "c", "a"))

// Reduction of Lists
def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)

concat(1::3::Nil, 2::4::Nil)