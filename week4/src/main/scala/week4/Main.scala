package week4

object Main {

	def main(args:Array[String]) = {
    /*
    println("week 4") 
    val one = singleton(1)
    println(one.head+" "+one.tail)
    val tr = singleton(true)
    println(tr.head+" "+tr.tail)
    */
    val list:List[Int] = cList(3, cList(2, cList(1, singleton(0))))
    println("n: "+nth(0, list))
    println("n: "+nth(0, singleton(9)))
    println("n: "+nth(0, new Nil[Int]))

  }

  def cList[T](elem:T, list:List[T]) = new Cons[T](elem, list)

  def singleton[T](elem:T) = new Cons[T](elem, new Nil[T])

  def nth[T](n:Int, list:List[T]):T = {
    def th[T](level:Int, l:List[T]):T = {
      if(l.isEmpty) throw new IndexOutOfBoundsException
      else if(level == n) l.head
      else th(level+1, l.tail)
    }
    th(0, list)
  }

}


//trait List[T]
 
//class Cons[T](val head:T, val trail:List[T]) extends List[T]
/*class Cons(_head:Int, _trail:IntList) extends IntList{
  val head = _head
  val tail = _tail
}*/

//class Nil[T] extends List[T]







