package week3

object Main {
	def main(args:Array[String]) = {
		val empty = Empty
		val tree7 = empty incl 7
		println(tree7)
		val tree5 = tree7 incl 5
		println(tree5)
		val tree12 = tree5 incl 12
		println(tree12)
		val tree9 = tree12 incl 9
		println(tree9)
		val tree13 = tree9 incl 13
		println(tree13)
		val other = new NonEmpty(6, Empty, Empty)
		val unionOther = other union tree13
		println(unionOther)
	}
}

abstract class IntSet{
	def incl(x:Int):IntSet
	def contains(x:Int):Boolean
	def union(other:IntSet):IntSet
}

object Empty extends IntSet{
	def incl(x:Int):IntSet = new NonEmpty(x, Empty, Empty)
	def contains(x:Int):Boolean = false
	def union(other:IntSet):IntSet = other
	override def toString = "."
}

class NonEmpty(elem:Int, left:IntSet, right:IntSet) extends IntSet{
	def contains(x:Int):Boolean = 
		if(x<elem) left contains x
		else if (x>elem) right contains x
		else true
	def incl(x:Int):IntSet = 
		if(x<elem) new NonEmpty(elem, left incl x, right)
		else if(x>elem) new NonEmpty(elem, left, right incl x)
		else this
	def union(other:IntSet):IntSet = {
		((left union right) union other) incl elem
	}
	override def toString = "{" + left + elem + right + "}"
}



