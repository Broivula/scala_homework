import java.io.File
import java.util.Scanner

// Exercises 1 b

// chapter 1)

// 5 What does 10 max 2 mean? In which class is the max method defined?
// basically it translates to a method call to the RichInt -class.
// the max -method is defined there, since it contains all sorts of extra methods for integers
// . could be written as 10.max(2)


// 6
var n : BigInt = BigInt(2).pow(1024)

// 8
var x : BigInt = BigInt(1024, rnd = scala.util.Random)
println(x.toString(36))

//9
var str = "Hello"
println(str.head)
println(str.last)

// chapter 2)

// 4 Write a Scala equivalent for the Java loop for(int i = 10; 1 >= 0; i--) System.out.println(i);
for(i <- 10 to 1 by -1) yield println(i)

// 5 Write a procedure countdown (n:Int) that prints the numbers from n to 0
def countdown(x: Int) = for(i <- x to 0 by -1)yield println(i)
countdown(15)

// 6 Write a for loop for computing the product of the Unicode codes of all..
def unicodeCounter(s: String): Long ={
  var v: Long = 1
  s.foldLeft(1L)(_ * _.toInt)
  for(i <- s) v *= i.toInt
  v
}
println(unicodeCounter("Hello"))

// 7 Solve the preceding exercise without a for loop
def unCounter(s:String): Long = {
  s.map(_.toLong).product
}
println(unCounter("Hello"))


// chapter 3)

// 2 Write a loop that swaps...
val a = Array(1, 2, 3, 4, 5).toBuffer
for(i<- 1 until a.length by 2){a.insert(i-1, a(i)); a.remove(i+1)}
for(i <- 0 until a.length)println(a(i))


// 3 same as before but with yield
val c = Array(1, 2, 3, 4, 5)
val b = for(i<- 0 until c.length by 2; j <- (i+1) to i by -1 if j < c.length)yield c(j)
for(i <- 0 until b.length)println(b(i))


// 5 How do you compute the average of Double array
val doubleArray : Array[Double] = Array(1.5, 2.5, 3.3, 4.7, 5.9)
println(doubleArray.sum / doubleArray.length)


// 7 Write code snippet that produces all values...
val testArr = Array(5, 13, 15, 13, 7, 29, 5, 68)
testArr.distinct


// chapter 4)

// 1 setup a map of gizmos...
var gizmoMap = Map("raspberryPi" -> 33.0, "piCamera" -> 39.0, "wireless keyboard" -> 49.0)
gizmoMap = for((k, v) <- gizmoMap) yield (k -> (v * 0.9))

// 2 write a program that reads text....
var wordMap = scala.collection.mutable.Map[String, Int]()
val in = new Scanner(new File("D:\\Koulujutut\\Scala\\tasks_23_10_2019\\src\\main\\scala\\textfile.txt"))
while(in.hasNext()){
  val word = in.next().toLowerCase().stripPrefix(",").stripSuffix(",").stripPrefix(".").stripSuffix(".")
  if(wordMap.keys.toArray.contains(word)){
    wordMap += (word -> (wordMap.getOrElse(word, 0) + 1))
  }else wordMap += (word -> 1)
}
for((k, v) <- wordMap) println(k, v)


// 3 repeat the exercise with an immutable map
var immutableWordMap = Map[String, Int]()
val reader = new Scanner(new File("D:\\Koulujutut\\Scala\\tasks_23_10_2019\\src\\main\\scala\\textfile.txt"))
while(reader.hasNext()){
  val word = reader.next().toLowerCase().stripPrefix(",").stripSuffix(",").stripPrefix(".").stripSuffix(".")
    immutableWordMap.contains(word) match {
    case true =>  immutableWordMap = immutableWordMap + (word -> (immutableWordMap.getOrElse(word, 0) + 1))
    case false => immutableWordMap = immutableWordMap + (word -> 1)
  }
}
for((k, v) <- immutableWordMap) println(k, v)


// 8 write a function minmax...
def minmax(values: Array[Int]) : (Int, Int) = {
  (values.reduceLeft(_ min _), values.reduceLeft(_ max _))
}
var valueArray = Array(1, 5, 66, 15, 69)
print(minmax(valueArray))


//  chapter 5)

// 1 improve the Counter class...
class Counter(var value: Int){
  def increment(){if (value + 1 == Int.MaxValue) value = 1 else value += 1}
  def current = value
}
var cd = new Counter(0)
cd.increment()
cd.value


// 2 write a bank account class..
class BankAccount(private var _balance: Int){
  def deposit(value: Int) {_balance += value}
  def withdraw(value: Int): Unit = if(value < _balance) _balance -= value
  def balance = _balance
}
var ba = new BankAccount(50)
ba.deposit(10)
ba.withdraw(30)
ba.balance

// 3 write a class Time..
class Time(private var _hours : Int, private var _minutes: Int){
  def before(other: Time) : Boolean = { this._hours < other._hours} || this._hours == other._hours && this._minutes < other._minutes
}
var t1 = new Time(15, 15)
var t2 = new Time(16, 12)
t1.before(t2)

// 6 In the Person class...
class Person(var age: Int, var name: String){
  if(age < 0) age = 0
}
var p1 = new Person(-5, "Kaapo")
p1.age

// 7 write a class Person with...
class Person2(fullname: String){
  private val nameHelper = fullname.split(" ")
  val first: String = nameHelper(0)
  val last: String = nameHelper(1)
}
val p2 = new Person2("Kelpo Koira")
p2.first
p2.last

// should the primary constructor parameter be var, val or a plain parameter?
// I think the best course would be the plain one, since we're not going to use that
// parameter for anything else. If we'd make it a var or val, there would be some getters and
// setters created under the hood, and we don't want that. That's just extra in our case

// 8 make a class car..
class Car(val manufacturer: String, val modelName: String, val modelYear: Int, var license: String){
  def this(manufacturer: String, modelName: String, license: String){ this(manufacturer, modelName, -1, license)}
  def this(manufacturer: String, modelName: String){this(manufacturer, modelName, -1, "")}
  def this(manufacturer: String, modelName: String, modelYear: Int){this(manufacturer, modelName, modelYear, "")}
}
// I would prefer (and always have) the constructor which takes all 4 values.
// firstly, with placeholder values you can ignore the ones that might now always be used
// and secondly, I think It just makes the code more readable, to have all the fields there.


// chapter 6)

// 1 write an object Conversions...
object Conversions{
  def inchesToCentimeters(value: Double) : Double = {value*2.54}
  def gallonsToLiters(value: Double) : Double = {value*3.785}
  def milesToKilometers(value: Double) : Double = {value*1.609}
}
Conversions.milesToKilometers(5)
Conversions.inchesToCentimeters(5)
Conversions.gallonsToLiters(5)

// 2 the preceding problem wasn't very oo...
abstract class UnitConversion(){def conversion(value: Double): Double}
object InchesToCentimeters extends UnitConversion {
  override def conversion(value: Double): Double = value*2.54
}
object GallonsToLiters extends UnitConversion{
  override def conversion(value: Double): Double = value*3.785
}
object MilesToKilometers extends UnitConversion{
  override def conversion(value: Double): Double = value*1.609
}
InchesToCentimeters.conversion(5)
GallonsToLiters.conversion(5)
MilesToKilometers.conversion(5)


// chapter 7)

// 4 why do you think the Scala language...
// Most likely because in Scala a package can be separated in multiple files, having variables
// and functions all over the place could get messy. This way the whole thing is more cleaner.

//5 what is the meaning of private[com]...
// private[com] makes it so that the method can only be called from within the com package.
// if you don't want anyone outside the com to give raises, it's a good thing, I guess
// ..not sure if that's what they were going after, but I can't think of anything else.


// chapter 9)
import scala.io.Source

// 3 write a scala code snippet that..
Source.fromFile("D:\\Koulujutut\\Scala\\tasks_23_10_2019\\src\\main\\scala\\textfile.txt").mkString.split(" ").filter(_.length > 12).foreach(println(_))


// 4 write a scala program that reads a text file containing only floating point...
val tokens =  Source.fromFile("D:\\Koulujutut\\Scala\\tasks_23_10_2019\\src\\main\\scala\\floatingpoints.txt").mkString.split("\\s+")
var numbers = for(e <- tokens)yield e.toDouble
println(numbers.sum)
println(numbers.sum/numbers.length)
println(numbers.min)
println(numbers.max)

