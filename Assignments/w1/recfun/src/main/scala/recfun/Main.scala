package recfun

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
  def pascal(c: Int, r: Int): Int =
    (c, r) match {
      case (c, r) if (c == 0 || c == r) => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def computeBalanceRecursive(remainingChars: List[Char], remainingParentheses: Int): Boolean = {
      if (remainingParentheses < 0) { false }
      else if (remainingChars.isEmpty) { remainingParentheses == 0 }
      else {
        remainingChars.head match {
          case '(' => computeBalanceRecursive(remainingChars.tail, remainingParentheses + 1)
          case ')' => computeBalanceRecursive(remainingChars.tail, remainingParentheses - 1)
          case _ => computeBalanceRecursive(remainingChars.tail, remainingParentheses)
        }
      }
    }

    computeBalanceRecursive(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (remMoney, remCoins) if (remMoney < 0 || remCoins.isEmpty) => 0
    case (remMoney, remCoins) => countChange(remMoney, remCoins.tail) + countChange(remMoney - remCoins.head, remCoins)
  }
}
