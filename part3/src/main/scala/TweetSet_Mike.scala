package objsets

import common._
import TweetReader._
import annotation.tailrec

/**
 * Класс твита.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * Класс представляет собой множество объектов `Tweet` в форме бинарного дерева
 *  
 * Заметим, что для того, чтобы храниться в дереве, твиты должны быть сравнимы. 
 * В этой реализации мы сравниваем тексты твитов. Поэтому в множестве не может храниться 
 * несколько твитов с одинаковым текстом. Детали работы бинарного дерева поиска смотрите в [1].
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * Возвращает подмножество `this`, для элементов которого  
   * предикат true.
   *
   * Вопрос: стоит ли реализовывать метод тут или его нужно сделать абстрактным? 
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  /**
   * Фильтр, сохраняющий промежуточный результат в аккумуляторе.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet 

  /**
   *  Возвращает `TweetSet` включающий элементы из `this` или `that`.
   *
   * Вопрос: стоит ли реализовывать метод здесь или лучше реализовать его в подклассах?
   */
   def union(that: TweetSet): TweetSet 

  /**
   * Возвращает наиболее цитируемый твит.
   *
   * Вызов `mostRetweeted` на пустом множестве должен кидать `java.util.NoSuchElementException`.
   *
   * Вопрос: стоит ли реализовывать метод здесь или лучше реализовать его в подклассах? 
   */
  def mostRetweeted: Tweet

  def moreRetweetedThan(mostRetweeted: Tweet): Tweet 

  /**
   * Возвращает множество твитов из текущего множества, отсортированное по количеству ретвитов в убывающем порядке. 
   *
   * Hint: используйте метод `remove` из TweetSet.
   *
   * Вопрос: стоит ли реализовывать метод здесь или лучше реализовать его в подклассах? 
   */
  def descendingByRetweet: TweetList = descendingByRetweetAcc(Nil).revert;
  def descendingByRetweetAcc(Acc:TweetList):TweetList 
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
/**
   ****************************************************************************************************************************
   */
class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  
  def union(that: TweetSet): TweetSet = that
  
  def moreRetweetedThan(mostRetweeted: Tweet): Tweet = mostRetweeted
  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException("No elements at all")
  def descendingByRetweetAcc(Acc:TweetList):TweetList = Acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}
/**
   ****************************************************************************************************************************
   */
class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  
  def union(that: TweetSet): TweetSet = this.right.union(this.left.union(that.incl(this.elem)))
   /**
   * в качестве аккумулятора для правого поддерева используем результат прохода по левому поддереву
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = if (p(elem)) right.filterAcc(p,left.filterAcc(p,acc.incl(elem)))
																else right.filterAcc(p,left.filterAcc(p,acc))
  def moreRetweetedThan(mostRetweeted: Tweet): Tweet = if (elem.retweets>mostRetweeted.retweets) this.right.moreRetweetedThan(this.left.moreRetweetedThan(elem))
														else this.right.moreRetweetedThan(this.left.moreRetweetedThan(mostRetweeted))
  def mostRetweeted: Tweet = this.moreRetweetedThan(elem)
  def descendingByRetweetAcc(Acc:TweetList):TweetList = {
		val mr = this.mostRetweeted
		this.remove(mr).descendingByRetweetAcc(Acc.add(mr))
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
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def revert:TweetList = this.revertAcc(Nil)
  def revertAcc(that:TweetList):TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  def add(tweet: Tweet) = new Cons(tweet, this)
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  def revertAcc(that:TweetList):TweetList = that 
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  def revertAcc(that:TweetList):TweetList = this.tail.revertAcc(that.add(this.head))
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  @tailrec
  def tweetContainWordFrom(l: List[String])(t: Tweet): Boolean =
    if (l.isEmpty) false
    else if (t.text.contains(l.head)) true
    else tweetContainWordFrom(l.tail)(t)

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter( tweetContainWordFrom(google) )
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter( tweetContainWordFrom(apple) )

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = appleTweets.union(googleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
