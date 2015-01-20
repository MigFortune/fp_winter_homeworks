package sets

object IntSet {
  /*
   *  Мы задаем множество характерестической функцией.
   *  Элементы, для которых предикат True, содержаться в множестве,
   *  False - нет
   */
  type Set = Int => Boolean

  /**
   * Проверка, входит ли элемент в множество
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
  /**
   * Функция создает множество, состоящее из одного элемента
   */

  def singletonSet(elem: Int): Set = (x:Int)=> if (x==elem) true else false

  /**
   * Объединение множеств, в результирующее множество
   * входят элементы из `s` или `t`
   */
  def union(s: Set, t: Set): Set = (x:Int)=> s(x)||t(x)

  /**
   * Пересечение множеств, только общиее для `s` или `t` элементы
   */
  def intersect(s: Set, t: Set): Set = (x:Int)=>s(x)&&t(x)

  /**
   * Вычитание, элементы из `s`, которых нет в `t`
   * the set of all elements of `s` that are not in `t`.
   */
  def sub(s: Set, t: Set): Set = (x:Int)=>s(x)&&(!t(x))
  def supp(s:Set):Set = (x:Int)=>(!s(x))
  /**
   * Элементы `s`, удовлетворяющие предикату `p`.
   */

  def filter(s: Set, p: Int => Boolean): Set = (x:Int)=>s(x)&&p(x)

  /**
   * Удовлетворяют ли все целые числа в отрезке [-bound; bound] из `s` предикату `p`.
   */
  def forall(s: Set, p: Int => Boolean, bound: Int): Boolean = {val t=supp(sub(s,filter(s,p)))
    if (bound<0)true else if(!(t(-bound)&&t(bound)))false else forall(s,p,bound-1) }
  /**
   * Существует ли в `s` целое в отрезке [-bound; bound],
   * удовлетворяющее `p`.
   */
  /**
  *def exists(s: Set, p: Int => Boolean, bound: Int): Boolean = {val t=intersect(s,filter(s,p))
  *if (bound<0)false else if(t(-bound)||t(bound))true else exists(s,p,bound-1) }
  */
  def exists(s: Set, p: Int => Boolean, bound: Int): Boolean = {def np(x:Int):Boolean = (!p(x))
  !forall(s,np,bound)}
  /**
   * Применяет `f` к каждому элементу `s` в отрезке [-bound; bound].
   */
  def map(s: Set, f: Int => Int, bound: Int): Set = (x:Int)=> {def Eq(n:Int):Boolean = (x==f(n))
  if (exists(s,Eq ,bound))true else contains(s, x)}


  /**
   * Displays the contents of a set
   */
  def toString(s: Set, bound: Int): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s, 1000))
  }
}
