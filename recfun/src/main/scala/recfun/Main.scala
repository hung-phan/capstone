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
    if ((c <= 0) || (c >= r)) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def stackBracket(bracketCount: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        bracketCount == 0
      } else {
        val newBracketCount: Int = chars.head match {
          case '(' => bracketCount + 1
          case ')' => bracketCount - 1
          case _ => bracketCount
        }

        if (newBracketCount < 0)
          false
        else
          stackBracket(newBracketCount, chars.tail)
      }
    }

    stackBracket(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def change(money: Int, coins: List[Int], count: Int): Int = {
      if (money < 0) {
        count
      } else if (coins.isEmpty) {
        if (money == 0) count + 1 else count
      } else {
        change(money, coins.tail, count) + change(money - coins.head, coins, count)
      }
    }

    change(money, coins, 0)
  }
}
