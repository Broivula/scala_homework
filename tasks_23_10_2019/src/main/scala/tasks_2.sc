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





