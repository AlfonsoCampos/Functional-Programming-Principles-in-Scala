// Other Collections
val xs = Array(1, 2, 3, 44)
xs map ( x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper)

s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs.unzip

s flatMap (c => List('.', c))

xs.sum
xs.max

val M = 10
val N = 10
(1 to M) flatMap ( x => (1 to N) map (y => (x,y)))

def scalarProduct(xs: Vector[Double], ys: Vector [Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum

def scalarProduct2(xs: Vector[Double], ys:Vector [Double]): Double =
  (xs zip ys).map{ case (x, y) => x * y }.sum

val a = Vector(1.0, 2.0, 3.0)
val b = Vector(2.0, 2.0, 2.0)

scalarProduct(a, b)
scalarProduct2(a, b)

def isPrime(n: Int): Boolean = (2 until n) forall (d => n%d != 0)
isPrime(5)

// Combinatorial Search & For Expressions
val n = 7
(1 until n) map (i =>
  (1 until i) map (j => (i,j)))

//(xss foldRight Seq[Int]()) (_ ++ _)

((1 until n) map (i =>
  (1 until i) map (j => (i,j)))).flatten

(1 until n) flatMap (i =>
  (1 until i) map (j => (i,j)))

(1 until n) flatMap (i =>
  (1 until i) map (j => (i,j))) filter (pair =>
  isPrime(pair._1 + pair._2))

// For-Expression
case class Person(name: String, age: Int)
val persons = List(new Person("Alfon", 33), new Person("Yolanda", 26))

persons filter (p => p.age > 30) map (p => p.name)
for (p <- persons if p.age > 30) yield p.name

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct3(xs: List[Double], ys: List[Double]): Double =
  (for ( (x,y) <- xs zip ys) yield x * y).sum

val c = List(1.0, 2.0, 3.0)
val d = List(2.0, 2.0, 2.0)

scalarProduct3(c, d)

// Combinatorial Search aka Sets
def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens

  queensWithRow forall {
    case(r, c) => col != c && math.abs(col - c) != row - r
  }
}

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
      queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
    } yield col :: queens

  placeQueens(n)
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

queens(4) map show

// Map

val romanNumbers = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry("US")
//capitalOfCountry("Andorra")
capitalOfCountry get "Andorra"
capitalOfCountry get "US"

def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("US")
showCapital("Andorra")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith (_.length < _.length)
fruit.sorted

fruit groupBy (_.head)

class Poly(val terms: Map[Int, Double]) {
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
    }
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

val cap1 = capitalOfCountry withDefaultValue "<unknown>"
cap1("Andorra")

class Poly2(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue(0.0)
  def + (other: Poly2) = new Poly2(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val p3 = new Poly2(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p4 = new Poly2(Map(0 -> 3.0, 3 -> 7.0))
p3 + p4
p3.terms(7)

class Poly3(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue(0.0)
  def + (other: Poly3) = new Poly3(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val p5 = new Poly3(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p6 = new Poly3(0 -> 3.0, 3 -> 7.0)
p5 + p6
p5.terms(7)

class Poly4(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue(0.0)
  def + (other: Poly4) = new Poly4((other.terms foldLeft terms)(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}

val p7 = new Poly4(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p8 = new Poly4(0 -> 3.0, 3 -> 7.0)
p7 + p8
p7.terms(7)

// Big Exercise
import scala.io.Source
val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' ->  "PQRS", '8' -> "TUV", '9' -> "WXYZ")

// Invert the mnem map to give a map from chars
val charCode: Map[Char, Char] =
  for {
    (digit, str) <- mnem
    ltr <- str
  } yield ltr -> digit


def wordCode(word: String): String =
  word.toUpperCase map charCode

wordCode("Java")

val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet

encode("7225247386")

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("7225247386")