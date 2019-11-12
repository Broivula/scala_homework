
abstract class IntList
case object Nil extends IntList
case class Cons (head: Int, tail: IntList) extends IntList

object IntList{
  def sum(list: IntList) : Int = {
    list match {
      case Cons(head, tail) =>  head + sum(tail)
      case Nil => 0
    }
  }

  def append(list1: IntList, list2: IntList) : IntList = {
    list1 match {
      case Cons(head, tail) => Cons(head, append(tail,list2))
      case Nil => list2
    }
  }

  def toString(list: IntList) : String = {
    list match{
      case Cons(head, tail) => head.toString + "," + toString(tail)
      case Nil => ""
   }
  }

  def reverse(list: IntList) : IntList = {
    list match{
      case Cons(head, tail) => reverse(Cons(head, tail))
      case Nil => list
    }
  }

  /*
  def filter(list: IntList, cond: Int => Boolean): IntList = {
    list match {
      case Cons(head, tail) => {
        if(cond(head)){

        }

      }
      case Nil => list
    }
  }
  */
}

object Main extends App {
  val s = Cons(3, Nil)
  val s2 = Cons(2, s)
  val s3 = Cons(1, s2)
  val e1 = Cons(6, Nil)
  val e2 = Cons(5, e1)
  val e3 = Cons(4, e2)

  println(IntList.sum(s3))
  println(IntList.toString(IntList.append(s3, e3)))
  println(IntList.toString(IntList.reverse(s3)))
}