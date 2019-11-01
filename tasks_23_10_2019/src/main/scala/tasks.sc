
/*
Exercises 1a

All exercises
*/
import scala.util._
import scala.math._
// 1)
val result = 8.*(4.*(3).+(6))./(2)

// 2)
var words = List("ha", "he", "ha", "ho")
println(listToString(words))
def listToString(words: List[String]): String  = {
  var s : String = ""
  words.foreach(word =>s += word + "and")
  s
}

// 3)
for(a <- 1 to 10; b <- 1 to 10; c <- 1 to 10; if pow(a,2)+ pow(b, 2) == pow(c, 2)) yield println(a, b, c)

// 4)

println(powerRec(2, 5))
  // a)
def powerLoop(x: Int, y: Int): Int = {
  var result = 1
  for(i <- 0 to y)result = result * x
  result
}
  // b)
def powerRec(x: Int, y: Int, result: Int = 1): Int = {
  y match {
    case 0 => result * x
    case _ => powerRec(x, y-1, result*x)
  }
}

// 5)
var lottoNumbers  = scala.util.Random.shuffle((1 to 40).toList).take(7)
println(lottoNumbers.sorted)

// 6)
class MyClass1J (private var name: String = "no name", private var credits: Int = 0){
  def getName = this.name
  def setName(name: String)= this.name = name
  def getCredits = this.credits
  def addCredits(credits: Int)= if (credits > (this.credits + 15)) this.credits += credits
}

val b = new MyClass1J("elias", 0)
b.addCredits(50)
println(b.getCredits)
b.setName("new_name")
println(b.getName)


// 7)

class MyClass2J(val name: String, val credits: Int = 0) {
  def this(credits: Int) {this("no name", credits)}
  def addCredits(credits: Int): Option[MyClass2J] = if (credits > this.credits + 15) None
  else Some(new MyClass2J(name, this.credits + credits))
  def getName = this.name
  def getCredits = this.credits
}

val o = new MyClass2J("bob", credits = 5)
var r = o.addCredits(10)
r match{
  case None => println("returned a none")
  case Some(value) => println(s"returned some value ${value.getName} ${value.getCredits}")
}


// 8)

object MyClass1J{
  private var amountOfInstances = 0
  def addNewInstance() = this.amountOfInstances += 1
  def apply(name:String = "no name was given", credits:Int = 0) = {
    println("running apply..")
    if(this.amountOfInstances < 3) {
      this.addNewInstance()
      new MyClass1J(name, credits)
    } else null
  }
}

val first = MyClass1J()
val second = MyClass1J()
val third = MyClass1J()
val fourth = MyClass1J()
if(third != null && fourth == null){println("fourth was null")}


// 9)

/*
So, which one of the classes would I prefer...
I honestly had better time with the first version. I feel like it's much more readable, straight forward, and does everything I want it to do.

Obviously the second class has it's better sides as well, such as the addition of auxiliary constructors, which I feel are a necessery evil
(boring to write, yet extremely important)

To be honest, I really didn't get the point of the second classes addCredits() -function. I mean, I understand It was to show that you
can choose to return a null value, if something goes awry, and that the Nothing -abstract class is a master to everyone. But since it's an abstract class,
I don't understand what the value of such as a return value would be. I thought the point of the addCredits() funcion was a bit silly in my implementation.
I mean, under those silly conditions, it might return a new instance of the Class. I feel like that wasn't the goal of that, rather maybe it was just for
messing around with the optionals in Scala. (as this is an exercise.)
*/

// 10)

object MarkerColor extends Enumeration{
  val Green, Blue, Red, Black, Yellow = Value
}
abstract class Pen(){
  def draw(): Option[Int]
  def uncap(): Boolean
}
class Marker(private val markerColor: MarkerColor.Value = MarkerColor.Black, private var inkAmount: Int = 5, private var isCapped: Boolean = true) extends Pen {
  override def draw(): Option[Int] = {
    if(!isCapped && getInkAmount > 0){
      println(s"drawing with ${markerColor.toString.toLowerCase} marker..")
      negateInk
      Some(this.inkAmount)
    }else{ None }
  }

  override def uncap(): Boolean = {
    this.isCapped = !this.isCapped
    println(s"the cap state is now ${if(this.isCapped)"closed" else "open"}")
    this.isCapped
  }

  def getIsCapped: Boolean = {this.isCapped}
  def negateInk = this.inkAmount -= 1
  def getInkAmount: Int = this.inkAmount
}

val m = new Marker()
m.uncap()
for (i<- 0 to 6) yield m.draw()
match {
  case Some(value) =>{
    println(s"..current ink amount is $value")
  }
  case None => {
    println(s"drawing failed, cap is ${if(m.getIsCapped)"on"else "off"} and the amount of ink is ${m.getInkAmount}")
  }
}