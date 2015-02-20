package huffman

import scala.annotation.tailrec

/**
* Задание 4: Huffman coding
*
*/

object Huffman {
implicit class MyList [T](L:List[T]) {
def reverseList:List[T] = {def reverseAcc(Ls:List[T],Acc:List[T]):List[T] = if (Ls==Nil) Acc else reverseAcc(Ls.tail,Ls.head::Acc)
	reverseAcc(L,Nil)
	}
def ListContains(el:T):Boolean = {def ListContainsAcc (Ls:List[T],el:T):Boolean = if (Ls==Nil) false  else if (Ls.head==el) true else ListContainsAcc(Ls.tail,el)
	ListContainsAcc (L,el)
	}
def InsertAscList (el:T,less: (T,T)=>Boolean):List[T] = if (L==Nil) el::Nil else 
	if (less(el,L.head)) List(el):::L else L.head::L.tail.InsertAscList(el,less)
} 
  /**
  * Код Хафмана представляется в виде бинарного дерева.
  *
  * Кждый листовой узел  `Leaf` представляет символ кодируемого алфавита.
  * Вес `Leaf` задает частоту появления символа в тексте.
  *
  * Ветви дерева, узлы `Fork`, кодируют множество всех букв, представляемых листьями этой ветви.
  * Вес `Fork` равен сумме всех ветвей
  */
  abstract class CodeTree(val weight: Int)
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], override val weight: Int) extends CodeTree(weight)
  case class Leaf(char: Char, override val weight: Int) extends CodeTree(weight)

 

  // Часть 1: Основа

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, weight) => weight
    case Fork(left, right, _, _) => weight(left) + weight(right)
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, _) => List(char)
    case Fork(left, right, _, _) => chars(left) ::: chars(right)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Часть 2: Генерирование дерева Хафмана

  /**
  * В этой части мы будем работать со списком симвалов. Эта функция позволяет создать список символов
  * из заданой строки.
  */
  def string2Chars(str: String): List[Char] = str.toList

  /**
  * Эта функция подсчитывает количиство фхождений каждого уникального символов из списка `chars` в этот же список.  
   * Например вызов
  *
  *   times(List('a', 'b', 'a'))
  *
  * должен вернуть следующее (порядок пар в списке не важен):
  *
  *   List(('a', 2), ('b', 1))
  *
  * Тип `List[(Char, Int)]` определяет список пар, в котором каждая пара включает символ и целое число вхождений этого символа
  * Пары могут быть созданы с помощью скобок:
  *
  *   val pair: (Char, Int) = ('c', 1)
  *
  * Для доступа к элементам пары можно использовать методы `_1` и `_2`:
  *
  *   val theChar = pair._1
  *   val theInt  = pair._2
  *
  * или сопоставление с образцом:
  *
  *   pair match {
    *     case (theChar, theInt) =>
    *       println("character is: "+ theChar)
    *       println("integer is  : "+ theInt)
    *   }
    */
    def times(chars: List[Char]): List[(Char, Int)] = {
		def AddChar(Acc:List[(Char,Int)],C:Char):List[(Char,Int)] = if (Acc==Nil) (C,1)::Nil	
		else if (Acc.head._1==C)  (C,Acc.head._2+1)::Acc.tail else Acc.head::AddChar(Acc.tail,C)
		def IterChars(Acc:List[(Char,Int)],LC:List[Char]):List[(Char,Int)] = if (LC==Nil) Acc else IterChars(AddChar(Acc,LC.head),LC.tail)
		IterChars(Nil,chars)	
		}
	
    /**
    * Возвращает список узлов `Leaf` для заданной таблицы частот `freqs`.
    *
    * Возвращаемый список должен быть отсортирован в возрастающем порядке (в голове списка
      * должна стоять пара с минимальным весом), где вес пары - это частота вхождения символа.
    */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
		def IterList(Acc:List[Leaf],unsorted:List[(Char, Int)]):List[Leaf] = if (unsorted==Nil) Acc 
			else IterList(Acc.InsertAscList(Leaf(unsorted.head._1,unsorted.head._2),(x:Leaf,y:Leaf)=>(weight(x)<weight(y))),unsorted.tail)
		IterList(Nil,freqs)
	}
	
    /**
    * Проверяет что `trees` включает только одно дерево.
    */
    def singleton(trees: List[CodeTree]): Boolean = if (trees == Nil) false
		else (trees.tail == Nil)
    /**
    * Параметр `trees` содержит список `CodeTree`, отсортированный в возрастающем порядке весов.
    *
    * Функция принимает первые два элемента `trees` и комбинирует их в один узел
    * `Fork`. Этот узел добавляется в `trees` на свое место согласно весу.
    *
    * Если в `trees` меньше двух элементов, то он возвращается не измененным.
    */
    def combine(trees: List[CodeTree]): List[CodeTree] = if ((trees == Nil)||(singleton(trees))) trees
		else trees.tail.tail.InsertAscList(makeCodeTree(trees.head,trees.tail.head),(x:CodeTree,y:CodeTree)=>(weight(x)<weight(y)))
    /**
    * Эта функция вызывается следующим образом:
    *
    *   until(singleton, combine)(trees)
    *
    * где `trees` имеет тип `List[CodeTree]`, `singleton` и `combine` реализованы
    * ранее.
    *
    * Вызов `until` из примера выше должен вызывать две функции до тех пор, пока в списке не останется один элемент.
    * затем должен вернуть этот список.
    */
    def until(finishCondition: List[CodeTree] => Boolean, combineFunction: List[CodeTree] => List[CodeTree] )(trees: List[CodeTree]): CodeTree = 
      if (finishCondition(trees)) trees.head
	  else until(finishCondition, combineFunction)(combineFunction(trees))

    /**
    * Эта функция создает оптимальное дерево Хафмана для списка символов `chars`.
    *
    * В аргументе `chars` простой текст. Эта функция определяет частоту символов в тексте
    * и создает дерево на основе этой информации.
    */
    def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))
	
    // Часть 3: Декодирование

    type Bit = Int

    /**
    * Эта функция декадирует послкдовательность бит `bits` используя дерево кодирования `tree` и возвращает
     * результат в виде списка символов.
    */

	
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
		def decodeAcc(acctree:CodeTree,accbits:List[Bit],accchars:List[Char]):List[Char] = acctree match { 
			case Leaf(char, _) =>  decodeAcc(tree,accbits,char::accchars)
			case Fork(left, right, _, _) => if (accbits==Nil) accchars else 
				if (accbits.head==0) decodeAcc(left,accbits.tail,accchars)
					else decodeAcc(right,accbits.tail,accchars)}
		decodeAcc(tree,bits,Nil).reverseList
	}
	

    /**
    * Дерево для французкого языка.
    * На основе данных
    *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
    */
    val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

    /**
    * Что это за сообщение? Можете декадировать?
    * Используйте `frenchCode`, определенное вверху.
    */
    val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

    /**
    * Напишите функцию декадирования зашифрованной фразы
    */
    def decodedSecret: List[Char] = decode(frenchCode,secret)


    // Часть 4a: Кодирование с использованием дерева Хаффмана

    /**
    * Кодирует `text` используя `tree`
    * в последовательность бит.
    */
	def encodeChar(tree:CodeTree) (c:Char):List[Bit]={
		def encodeCharAcc (ATree:CodeTree)(c:Char,Acc:List[Bit]):List[Bit] = ATree match  { 
			case Leaf(char, _) =>  if (char==c) Acc.reverseList else Nil
			case Fork(left, right, _, _)=> if (chars(left).ListContains(c)) encodeCharAcc(left)(c,0::Acc)  
											else if (chars(right).ListContains(c)) encodeCharAcc(right)(c,1::Acc) else Nil  
		}
		encodeCharAcc(tree)(c,Nil)
	}
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
		if (text==Nil) Nil else encodeChar(tree)(text.head):::encode(tree)(text.tail)
	}
    // Часть 4b: Кодирование с использованием таблицы

    type CodeTable = List[(Char, List[Bit])]

    /**
    * Функция возвращает последовательность бит для символов `char` в таблице `table`.
    */
    def codeBits(table: CodeTable)(char: Char): List[Bit] =
      if (table.isEmpty) List()
      else if (table.head._1 == char) table.head._2
      else codeBits(table.tail)(char)

    /**
    * Строит таблицу, включающую каждый символ в дереве.
    * Последовательность бит представляет символ.
    */
    def convert(tree: CodeTree): CodeTable = {
	 def iterconvert(charlist:List[Char]):CodeTable = if (charlist==Nil) Nil 
		else (charlist.head,encodeChar(tree)(charlist.head))::iterconvert(charlist.tail)
	 iterconvert(chars(tree))
	}

    /**
    * Объеденяет две таблицы символов в одну. В зависимости от того, как
    * вы используете этот метод в `convert`, объявленый сверху, этот метод может выполнять трансформации
    * над вргументами.
    */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

    /**
    * Кодирует `text` согласно `tree`.
    *
    * Для ускорение кодирования сначала трансформирует дерево в таблицу
    * и только потом выполняет кодирование.
    */
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
		def IterEncode (cl:List[Char]):List[Bit] = if (cl==Nil) Nil else 
			codeBits(convert(tree))(cl.head):::IterEncode(cl.tail)
		IterEncode(text)
	}
  }
