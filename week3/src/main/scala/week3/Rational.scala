package week3

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


