package funsets

object Main extends App {
  import FunSets._
  //println(contains(singletonSet(1), 1))
  val s1 = singletonSet(1)
  val filterSet = filter(s1, x=>x>1)
  printSet(filterSet)
}
