package higher

import math.abs

object Higher {

	def main(args: Array[String]):Unit = {
		println("Higher order functions")
		/*
		println(sumCrr2(x=>x)(3,4))
		println(sumInts(3,4))
		println(sumCubes(3,4))
		println(sumFactorials(3,4))
		println(product(x=>x*x)(3,4))
		println(factProduct(5))
		println(productMR(x=>x*x)(3,4))
		println(sumCrr(x=>x)(3,4))
		println(sumIntsTail(3,4))
		println(sumIntsLoopTail(x=>x,3,4))
		println(sumIntsAn(3,4))
		println(sumCubesAn(3,4))
		println(sumInts(3,4))
		println(sumCubes(3,4))
		println(sumFactorials(3,4))
		*/
		//println(fixedPoint( x=>1+x/2 )(1))
		//println(sqrt(2))
		//println(sqrtAD(2))
		val x = new Rational(1,3)
		val y = new Rational(5,7)
		val z = new Rational(3,2)
		//val result = x.add(y).mul(z) 
		val result = x + y mul z  
		println("result: "+result)
		println(x - y - z)
		//println(x.less(y))
		println(x < y)
		println(x max y)
		println(x.max(y))
		/*
		val strange = new Rational(1,0)
		val strangeDiv = strange.add(strange)
		println(strangeDiv)
		*/
		new Rational(1)
		println(new Rational(1,2).numer)
		//println(new Rational(1,2).less(new Rational(2,3)))
		println(new Rational(1,2) < new Rational(2,3))
	}

	class Rational(x:Int, y:Int) {
		require(y != 0, "denominator must be nonzero")

		def this(x:Int) = this(x, 1)

		private def gcd(a:Int, b:Int):Int = if(b==0) a else gcd(b, a % b)
		//private val g = gcd(x, y)
		def numer = x //    / g
		def denom = y //    / g
	
		//override def toString = numer + "/" + denom
		override def toString = {
			val g = gcd(numer, denom)
			numer/g + "/" + denom/g
		}

		//def add(that:Rational) = 
		//	new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
		def + (that:Rational) = 
			new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
		
		//def neg =
		def unary_-/*HERE IS A SPACE BEFORE EQUALS SIGN*/ =
			new Rational(-1 * numer, denom)	

		//def sub(that:Rational) = 
		def - (that:Rational) = 
			//add(that.neg)//new Rational(numer * that.denom - that.numer * denom, denom * that.denom)
			//this + that.neg
			this + -that

		def mul(that:Rational) = 
			new Rational(numer * that.numer, denom * that.denom)
		
		//def less(that:Rational) = numer * that.denom < that.numer * denom
		def < (that:Rational) = numer * that.denom < that.numer * denom

		def max(that:Rational) = if(this < that) that else this

	}

	def sqrtAD(x:Double) = fixedPoint(averageDamp(y=>x/y))(1.0)
	
	def sqrt(x:Double) = fixedPoint(y=>(y+x/y)/2)(1.0)

	val tolerance = 0.0001

	def isCloseEnough(x:Double, y:Double) = 
		abs( (x-y)/x ) / x < tolerance

	def fixedPoint(f:Double=>Double)(firstGuess:Double) =  {
		def iterate(guess:Double):Double = {
			val next = f(guess)
			if(isCloseEnough(guess, next)) next
			else iterate(next)
		}
		iterate(firstGuess)
	}
	
	def averageDamp(f:Double => Double)(x:Double) = (x+f(x)) / 2

	def productMR(f:Int=>Int)(a:Int,b:Int):Int = mapReduce(f, (x,y)=>x*y, 1)(a,b)

	def mapReduce(f:Int=>Int, combine:(Int,Int)=>Int, zero:Int)(a:Int,b:Int):Int =
		if(a>b) zero
		else combine( f(a) , mapReduce(f, combine, zero) (a+1, b) )

	def product(f:Int=>Int)(a:Int, b:Int):Int = 
		if(a > b) 1
		else f(a) * product(f)(a+1, b)

	def factProduct(n:Int):Int = product(x=>x)(1,n) 

	def sumCrr2(f:Int=>Int)(a:Int, b:Int):Int = 
		if(a>b) 0 else f(a) + sumCrr2(f)(a+1, b)

	def sumCrr(f:Int=>Int): (Int, Int)=>Int = {
		def sumF(a:Int, b:Int):Int = 
			if(a>b) 0
			else f(a) + sumF(a+1,b)
		sumF
	}
	
	def sumIntsCrr = sumCrr(x=>x)
	def sumCubesCrr = sumCrr(x=>x*x*x)
	def sumFactorailsCrr = sumCrr(fact)

	def sumIntsLoopTail(f:Int=>Int, a:Int, b:Int):Int = {
		def loop(a: Int, acc:Int):Int = {
			if(a > b) acc
			else loop(a+1, acc+f(a))
		}
		loop(a, 0)
	}

	def sumTail(f: Int => Int, a:Int, b:Int, total:Int):Int = {
		if(a>b) total
		else sumTail(f, a+1, b, total+a)
	}

	def sumIntsTail(a:Int, b:Int) = sumTail(x=>x, a, b, 0)

	def sum(f: Int => Int, a: Int, b:Int):Int = {
		if(a>b) 0
		else f(a) + sum(f, a+1, b)
	}

	def sumInts(a:Int, b:Int):Int = sum(id, a, b)
	def sumCubes(a:Int, b:Int):Int = sum(cube, a, b)
	def sumFactorials(a:Int, b:Int) = sum(fact, a, b)

	def id(x:Int):Int = x
	def cube(x:Int):Int = x*x*x
	def fact(x:Int):Int = {
		if(x==0) 1
		else x * fact(x-1)
	}
	
	def sumIntsAn(a:Int, b:Int) = sum(x=>x, a, b)
	def sumCubesAn(a:Int, b:Int) = sum(x=>x*x*x, a, b)

}
