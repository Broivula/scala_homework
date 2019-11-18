
// 1
object IntList{
  def apply (list : List[Int]) : IntList = {
    list.head match{
      case n : Int => {
        Cons(n, apply(list.tail))
      }
    }
  }
}

// 2

abstract class Vehicle(val serial: String,val door_amount : Int){
  println(s" from superclass! serial : $serial and amount of doors: $door_amount")
}

trait Printer {
  println("constructing Printer, which is a parent trait to ColorPrinter trait!")
  def print_msg(msg: String){println(msg)}
}

trait ColorPrinter extends Printer{
  println("constructing ColorPrinter trait!")
  override def print_msg (color: String) {println(color)}
}

class Car(override val door_amount: Int, override val serial: String) extends Vehicle(serial = serial, door_amount = door_amount) with ColorPrinter{
  println(s"from within the child class: door amount : $door_amount and serial $serial")
}


// 3

class Fraction(n: Int, d: Int) {
  private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
  private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
  override def toString = num + "/" + den
  def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
  def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)
  def *(other: Fraction) = new Fraction(num * other.num, den * other.den)
  def +(other: Fraction) = new Fraction(num + other.num, den)
  def -(other: Fraction) = new Fraction(num - other.num, den)
  def /(other: Fraction) = new Fraction(num * other.num, other.den * den)
}

object Fraction {  def apply(n: Int, d: Int) = new Fraction(n, d)}


// 4 (didn't fully understand this one, here's my solution)
abstract class Expr{
  implicit def operation (op: Expr) : Int = {
    op match{
      case Number(n) => n
      case Sum(n1, n2) => n1.operation(n1) + n2.operation(n2)
      case Mul(n1, n2) => n2.operation(n1) * n2.operation(n2)
    }
  }
}
case class Number(n: Int) extends Expr
case class Sum(n1: Expr, n2: Expr) extends Expr
case class Mul(n1: Expr, n2: Expr) extends Expr




object Main extends App {
  // for 1
  val il = IntList(List(1, 2, 3))
  println(IntList.toString(il))

  // for 2
  val c = new Car(5, "12345")
  println(c.serial)


  // for 3
  val f = new Fraction(1,2)
  implicit def fractionIntegerCalc(n: Int) = Fraction(n  , 1)

  println(f + 2)
  println(f - 2)
  println(f / 2)


  // for 4
  implicit def expr2int (n: Expr) : Int = n.operation(n)

  val n1 = Number(9)
  val n2 = Number(2)
  val nsum = Sum(n1, n2)
  val nmul = Mul(n1, n2)

  println(n1 + n2)
  println(n1 * n2)
  println(nsum + nsum)
  println(nmul * nmul)
  println(n2 + 5)
}
