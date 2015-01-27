package exercise1

object Exercise1 {
  /**
   * Exercise 1
   *
   * Теугольник Паскаля - это треугольник, в котором по краям стоят 1, а все остальные
   * элементы равны сумме двух вышестоящих:
   *
   *        1
   *       1 1
   *      1 2 1
   *     1 3 3 1
   *    1 4 6 4 1
   *    .........
   *
   * Эта функция производит расчет элемента треугольника Паскаля в строке `r` и горизонтальной позиции `c`.
   */
  def pascal(c: Int, r: Int): Int = if (r<0) 0 else if ((c<0)||(c>r)) 0 else if (r==0) 1 else pascal(c-1,r-1)+pascal(c,r-1)
  /**
   * Exercise 2
   *
   * Функция проверки балансировки скобок. Если в строке каждой открывающейся скобке соответсвует закрывающееся
   * (например (), (sdsadasd), (dsd)(sd)sdsdsd()) то результат true, иначе false (%-), )( и т.д.).
   */
  def balance(chars: List[Char]): Boolean = {def CountBr(chars: List[Char],Acc:Int):Int = if ((Acc>0)||chars.isEmpty) Acc else if (chars.head=='(')CountBr(chars.tail, Acc-1) else if (chars.head==')' )CountBr(chars.tail, Acc+1)else CountBr(chars.tail, Acc)
                                             CountBr(chars,0)==0}     
  /**
   * Exercise 3
   *
   * Размен заданной суммы задаными монетами.
   *
   * Функция определяет, сколькими способами можно разменять сумму `money` монетами из списка `coins`
   * Например 4 можно разменять 3 способами монетами 1 и 2: 1+1+1+1, 1+1+2, 2+2.
   *
   * В работе могут прегодиться следующие методы класса List: isEmpty, head и tail
   *
   * Совет: начните рассуждать с вырожденного случая когда нужно разменять нулевую сумму,
   * когда нужно разменять сумму 0 монетами
   *
   */
  def countChange(money: Int, coins: List[Int]): Int = {def IterCoin (Num:Int,money:Int,coins:List[Int]):Int = if ((Num<0)||(coins.head<=0)) 0 else IterCoin (Num-1,money,coins) + countChange(money-Num*coins.head,coins.tail)
	 if ((money!=0)&&coins.isEmpty)0 else if (money==0) 1 else IterCoin(money/coins.head,money,coins)}
}
