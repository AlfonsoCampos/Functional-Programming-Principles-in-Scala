"Hello World"
1+2

/* Recursion */
def abs(x: Double) = if (x < 0) - x else x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) =
    abs(guess * guess - x) < 0.001 * x

  def improve(guess: Double) =
    (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(2)
sqrt(4)

sqrt(0.001)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e50)

def factorial(n:Int): Int= {
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc else loop(acc * n, n - 1)
  loop(1, n)
}

factorial(4)

/* High Order functions */
def sum(f: Int=>Int, a: Int, b: Int) ={
  def loop(a:Int, acc:Int): Int =
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  loop(a, 0)
}

sum(x => x * x, 3, 5)

/* Currying */
def product(f: Int => Int) (a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

product(x => x * x)(3, 4)

def fact(n: Int) = product(x => x)(1, n)

fact(4)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int) (a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

mapReduce(x => x, (a, b) => a * b , 1) (3, 4)

def product2(f: Int => Int) (a: Int, b: Int): Int = mapReduce(f, (a, b) => a * b , 1)(a,b)
product2(x => x * x)(3, 4)

/* Fixed Point */
val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance
def fixedPoint(f:Double => Double) (firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}
fixedPoint( x => 1 + x/2)(1)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

def sqrt2(x: Double) = fixedPoint(averageDamp(y => x / y))(1)
sqrt2(2)

/* Classes */
class Rational(x: Int, y:Int) {
  require(y != 0, "denominator must not be zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val numer = x // gcd(x, y)
  val denom = y // gcd(x, y)

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  override def toString = numer.toString + '/' + denom.toString
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

//val strange = new Rational(1, 0)

x.numer
x.denom
x + y

x - y - z

/* Data Abstraction */
y + y
x < y
x max y
-x

//strange.add(strange)
new Rational(2)