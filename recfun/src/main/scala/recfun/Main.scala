package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _)           => 1
    case (a, b) if a == b => 1
    case (_, _)           => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_count(chars: List[Char], count: Int): Boolean =
      if (count < 0) false
      else if (chars.isEmpty && count == 0) true
      else if (chars.isEmpty && count > 0) false
      else (chars.head, chars.tail) match {
        case ('(', t) => balance_count(t, count + 1)
        case (')', t) => balance_count(t, count - 1)
        case (h, t)   => balance_count(t, count)
      }
    balance_count(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else
      (coins.head, coins.tail) match {
        case (h, t) =>
          countChange(money, t) + countChange(money - h, coins)
      }
}
