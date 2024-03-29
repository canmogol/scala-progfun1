package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    ""+retweets
    //"User: " + user + "\n" +
    //"Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def union(that: TweetSet): TweetSet 
  
  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def mostRetweeted: Tweet  
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
    def descendingByRetweet: TweetList 
  
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
    
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc 

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = null 

  def descendingByRetweet: TweetList = Nil
 
  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  override def toString = "."

}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

   
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = 
        if(p(elem)) acc.incl(elem).union(left.filter(p)).union(right.filter(p))
        else left.filter(p).union(right.filter(p))

  def union(that: TweetSet): TweetSet = {
        (left union (right union that)) incl elem
  }
 
  def mostRetweeted: Tweet = {
    var cn = new Cons(elem, Nil)
    foreach((t)=>
        if(t.retweets>cn.head.retweets){
          cn = new Cons(t, Nil)
        })
    cn.head
  }

  def descendingByRetweet: TweetList = {
    def x(cs:TweetSet, tl:TweetList):TweetList = {
      val mrt = cs.mostRetweeted
      if(mrt != null){
        val set = cs.remove(mrt)
        x(set, new Cons(mrt, tl))
      }else{
        tl
      }
    }
    val tl = x(this, Nil)
    var tr:TweetList = Nil
    tl.foreach((t)=>{
      tr = new Cons(t, tr)
    })
    tr
  }

 
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def toString = "{" + left + elem + right + "}"

}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = {
    val allTweets: TweetSet = TweetReader.allTweets
    val googleTweets = allTweets.filter((t)=>{
      google.exists(keyword=>t.text.contains(keyword))
    })
    googleTweets  
  }

  lazy val appleTweets: TweetSet = {
    val allTweets: TweetSet = TweetReader.allTweets
    val appleTweets = allTweets.filter((t)=>{
      apple.exists(keyword=>t.text.contains(keyword))
    })
    appleTweets
  } 
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = {
    googleTweets.union(appleTweets).descendingByRetweet
  } 
}

object Main extends App {
  // Print the trending tweets
  //GoogleVsApple.trending foreach println
  

  val n1 = ct("n1","10",10, new Empty, new Empty)
  val n2 = ct("n2","200",200, new Empty, new Empty)
  val n3 = ct("n3","3000",3000, new Empty, new Empty)
  
  val n12 = n1.union(n2) 
  val n123 = n12.union(n3)
  println("n12  "+n12)
  println("n123 "+n123)
  

  println("")
  println("filter n1    "+n1.filter((t)=>(t.retweets<10)))
  println("filter n1+   "+n1.filter((t)=>(t.retweets<11)))
  println("")
  println("filter n12   "+n12.filter((t)=>(t.retweets<200)))
  println("filter n12+  "+n12.filter((t)=>(t.retweets<201)))
  println("")
  println("filter n123  "+n123.filter((t)=>(t.retweets<3000)))
  println("filter n123+ "+n123.filter((t)=>(t.retweets<3001)))
  println("filter n123m "+n123.mostRetweeted)

  val n4 = ct("n4","4000",4000, new Empty, new Empty)
  val n5 = ct("n5","5000",5000, new Empty, new Empty)

  val n54123 = n5.union(n4).union(n123) 
  println("")
  println("filter n12345  "+n54123)
  println("filter n12345m "+n54123.mostRetweeted)


  n54123.descendingByRetweet.foreach((t)=>println("mrt> "+t.retweets))

  val allTweets: TweetSet = TweetReader.allTweets
  allTweets.foreach((t)=>println("t>"+t.retweets))






  def ct(user:String, text:String, retweets:Int, left:TweetSet, right:TweetSet) = new NonEmpty(new Tweet(user,text,retweets), left, right)

}
