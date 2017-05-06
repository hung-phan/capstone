def pascal(c: Int, r: Int): Int = {
  if ((c <= 0) || (c >= r))
    1 else
    pascal(c - 1, r - 1) + pascal(c, r - 1)
}

def balance(chars: List[Char]): Boolean = {
  def stackBracket(bracketCount: Int, chars: List[Char]): Int = {
    if (chars.isEmpty && bracketCount >= 0) {
      bracketCount
    } else {
      stackBracket(chars.head match {
        case '(' => bracketCount + 1
        case ')' => bracketCount - 1
        case _ => bracketCount
      }, chars.tail)
    }
  }

  stackBracket(0, chars) == 0
}

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

balance("(if (zero? x) max (/ 1 x))".toList)
