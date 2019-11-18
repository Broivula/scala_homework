// Author : Elias Koivula
// Student ID : 1706143
// DONE :  1 - 25 (not the last extra lotto one)

// 1)

def ismin(v:Int, list: List[Int], minList : Option[Int] = None): Boolean = {
  list match {
    case x :: tail => {
     if(minList.isEmpty) ismin(v, tail, Option(x))
      else{
       if(x < minList.get) ismin(v, tail, Option(x))
       else ismin(v, tail, minList)
     }
    }
    case _ => minList.get == v
  }
}

val list1 = List(7, 5, 8, 2, 1)
ismin(2, list1)
ismin(6, list1)
ismin(1, list1)

// 2)

def isinstring(c: Char, str: String): Boolean ={
  str.head match {
    case x : Char => {
      if(x.toLower == c.toLower) true
      else {
        if(str.tail.headOption.nonEmpty)isinstring(c, str.tail)
        else false
      }
    }
  }
}
isinstring('C', "puck")
isinstring('C', "Word")
isinstring('t', "plant")

// 3)

def cpos(c: Char, str: String, acc : Int = 1) : Option[Int] = {
  str.head match{
    case x : Char => if(x.toLower == c.toLower) Option(acc)else cpos(c, str.tail, acc+1)
    case _ => None
  }
}
println(cpos('C', "Crikey"))
println(cpos('C', "Attack"))

// 4)
import java.lang.Math.floorMod
def shiftc(c: Char, shift: Int, str: String): Option[Char] = {
  if(isinstring(c, str)){
    val n = cpos(c, str).get
      val newCharPos = n + shift
      if(newCharPos > str.length) Option(str(floorMod(newCharPos, str.length) - 1))
      else Option(str(newCharPos -1))
  }else None
}
println(shiftc('C', 6, "Crikey"))
println(shiftc('C', 8, "Crikey"))
println(shiftc('F', 2, "Crikey"))
println(shiftc('C', 2, "Crikey"))


// 5)
val alphabet = for(i <- 'a' to 'z')yield i
def ccipher(str: String, voca: String =""): String ={
  if(str.headOption.nonEmpty){
    str.head match{
      case x : Char =>{
        val newChar = shiftc(x, 13, alphabet.mkString(""))
        ccipher(str.tail, voca + newChar.get)
      }
      case _ => voca
    }
  }else voca
}

println(ccipher("abc"))

abstract class IntList
case object Nil extends IntList
case class Cons (head: Int, tail: IntList) extends IntList

object IntList {
  // 6

  def sum(list: IntList): Int = {
    list match {
      case Cons(head, tail) => head + sum(tail)
      case Nil => 0
    }
  }

  // 7
  def append(list1: IntList, list2: IntList): IntList = {
    list1 match {
      case Cons(head, tail) => Cons(head, append(tail, list2))
      case Nil => list2
    }
  }

  def toString(list: IntList): String = {
    list match {
      case Cons(head, tail) => head.toString + "," + toString(tail)
      case Nil => ""
    }
  }

  // 8
  def reverse(list: IntList): IntList = {
    list match {
      case Cons(head, tail) => append(reverse(tail), Cons(head, Nil))
      case Nil => list
    }
  }

  // 9
  def picknth(n: Int, list: IntList): Option[Int] = {
    list match {
      case Cons(head, tail) => if (n == 0) Option(head)
      else picknth(n - 1, tail)

      case Nil => None
    }
  }

  // 10
  def filter(list: IntList, cond: Int => Boolean): IntList = {
    list match {
      case Cons(head, tail) => {
        if (cond(head)) Cons(head, filter(tail, cond))
        else filter(tail, cond)
      }
      case Nil => list
    }
  }

  def min(list: IntList, smallest: Option[Int] = None): Option[Int] = {
    list match {
      case Nil => smallest
      case Cons(head, tail) => {
        if (smallest.nonEmpty) {
          if (head <= smallest.get) min(tail, Option(head))
          else min(tail, smallest)
        } else min(tail, Option(head))
      }
    }
  }

  def sort(list: IntList): IntList = {
    list match {
      case Cons(head, tail) => {
        Cons(min(list).get, sort(filter(list, _ > min(list).get)))
      }
      case Nil => list
    }
  }

  def get_tail(list: IntList): Option[IntList] = {
    list match {
      case Cons(_, tail) => Option(tail)
      case Nil => Option(list)
    }
  }

  def count(list: IntList): Int = {
    list match {
      case Cons(_, tail) => 1 + count(tail)
      case Nil => 0
    }
  }

  def median(list: IntList): Int = {
    picknth(count(list) / 2, list).get
  }

  def quick_shuffle(list: IntList): IntList = {
    list match{
      case Cons(head, tail) => {
        val random = scala.util.Random.nextInt(count(list))
        val num = picknth(random, list).get
        if(random == 0){
          if(count(list) != 1) Cons(num, quick_shuffle(tail))
          else list
        }
        else Cons(num, quick_shuffle(filter(list,_ != num)))
      }
      case Nil => list
    }
  }

}

object Main extends App {
  val e4 = Cons(7, Nil)
  val s = Cons(3, Nil)
  val s2 = Cons(2, s)
  val s3 = Cons(1, s2)
  val e1 = Cons(6, e4)
  val e2 = Cons(5, e1)
  val e3 = Cons(4, e2)

  println(IntList.sum(s3))
  println(IntList.toString(IntList.append(s3, e3)))
  println(IntList.toString(IntList.reverse(s3)))
  println(IntList.picknth(2, s3).get)
  println(IntList.toString(IntList.filter(s3, _ < 3)))
  println(IntList.min(e3))
  println(IntList.toString(IntList.sort(IntList.reverse(s3))))
  println(IntList.median(s3))

}

abstract class IntBinTree
case object Empty extends IntBinTree
case class Node(value: Int, left: IntBinTree, right: IntBinTree) extends IntBinTree

object IntBinTree {

  // 11
  def count(tree: IntBinTree): Int = {
    tree match {
      case Empty => 0
      case Node(_, left, right) => {
        1 + count(left) + count(right)
      }
    }
  }

  // 12
  def height(tree: IntBinTree) : Int = {
    tree match {
      case Empty => 0
      case Node(_, left : IntBinTree, right : IntBinTree) => {
        if(left != Empty || right != Empty)
          math.max(1 + height(left), 1 + height(right))
        else 1
      }
    }
  }

  // 13
  def isin(tree: IntBinTree, e: Int) : Boolean = {
    tree match {
      case Node(value, left, right) => {
        isin(left, e) || isin(right, e) || value == e
      }
      case Empty => false
    }
  }

  def t_min(tree :IntBinTree, e : Option[Int] = None) : Int = {
    tree match{
      case Node(value, left, right) => {
        Math.min(t_min(left, Option(Math.min(value, e.getOrElse(value+1)))), t_min(right, Option(Math.min(value, e.getOrElse(value+1)))))
      }
      case Empty => e.getOrElse(10)
    }
  }

  // 14
  def tolist(tree: IntBinTree) : IntList = {
    tree match{
      case Node(value, left, right) => {
        IntList.sort(Cons(value, IntList.append(tolist(left), tolist(right))))
      }
      case Empty => Nil
    }
  }

  // 15 & 16

  def fromlist(list: IntList, ite : Int = 0) : IntBinTree = {
    list match{
      case Cons(head, tail) => {
        if(ite % 2 == 0)Node(head, fromlist(tail, ite+1), Empty)
        else Node(head, Empty, fromlist(tail, ite+ 1))
      }
      case Nil => Empty

    }
  }

  def mult_str(str: String, n: Int) : String = {
    val s = for(i <- 0 to n)yield str
    s.mkString("")
  }

}

object Bin_Main extends App {
  val ib4 = Node(5, Empty, Empty)
  val ib3 = Node(4, Empty, ib4)
  val ib1 = Node(3, Empty, Empty)
  val ib2 = Node(2, ib3, Empty)
  val rootIb = Node(1, ib1, ib2)
  val e4 = Cons(7, Nil)
  val e1 = Cons(6, e4)
  val e2 = Cons(5, e1)
  val e3 = Cons(4, e2)

  println(IntBinTree.count(rootIb))
  println(IntBinTree.height(rootIb))
  println(IntBinTree.isin(rootIb, 6))
  println(IntBinTree.isin(rootIb, 3))
  println(IntBinTree.t_min(rootIb))
  println(IntList.toString(IntBinTree.tolist(rootIb)))
  println(IntBinTree.fromlist(IntList.quick_shuffle(e3)))

}

// 17
abstract class IntStack
case object EmptyStack extends IntStack
case class IStack(int: Int, tail: IntStack) extends IntStack

object IntStack{

  // 18
  def push(e: Int, s: IntStack) : IntStack = {
    IStack(e, s)
  }

  // 19
  def pop(s: IntStack) : IntStack = {
    s match {
      case IStack(head, tail) => tail
      case EmptyStack => EmptyStack
    }
  }

  // 20
  def top (s: IntStack) : Option[Int] = {
    s match {
      case IStack(head, _) => Option(head)
      case EmptyStack => None
    }
  }

  // 21
  def rot (s: IntStack) : IntStack = {
    s match {
      case IStack(head, tail) =>{
        push(top(tail).get,push(head, pop(tail)))
      }
      case IStack(_, EmptyStack) => s
      case EmptyStack => EmptyStack
    }
  }

  // 22
  def fold_fac(n: Int) : Int = {
    (n to 1 by -1).toList.foldLeft(1)((acc, e) => acc * e)
  }
  // 23
  def fold_isinstring(c: Char, s: String) : Boolean  = {
    s.toList.foldLeft(false)( _ || _.toString == c.toString)
  }

  // 24
  def fold_uppercase_a_to_z () : String = {
    val alphabet = for(i <- 'a' to 'z')yield i
    alphabet.mkString("").foldLeft("")((s, c) => {
      val r = scala.util.Random.nextInt(2)
      if(r == 0)s + c.toUpper
      else s + c
    })
  }

  // 25
  def fold_upper_in_s(s: String) : Int = {
    s.foldLeft(0)((v, c) => if(c == c.toUpper)v + 1 else v)
  }

}



object Play extends App{
  val a1 = IntStack.push(1, EmptyStack)
  val a2 = IntStack.push(2, a1)
  val a3 = IntStack.push(3, a2)
  val a4 = IntStack.push(4, a3)
  val f_ar = 5 to 1 by -1
  println(a4)
  println(IntStack.top(a4))
  println(IntStack.pop(a4))
  println(IntStack.rot(a4))
  println(f_ar.toList)


  println(IntStack.fold_fac(5))
  println(IntStack.fold_isinstring('c', "hockey"))
  println(IntStack.fold_isinstring('d', "hockey"))
  println(IntStack.fold_uppercase_a_to_z())
  println(IntStack.fold_upper_in_s("ThReE"))

}



