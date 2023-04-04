package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) {
      1
    } else if (r == 0) {
      c
    }
    else {
      (r * pascal(c-1, r-1)) / c
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {count == 0}
      else if (count < 0) false
      else if (chars.head == '(') loop(chars.tail, count + 1)
      else if (chars.head == ')') loop(chars.tail, count - 1)
      else loop(chars.tail, count)
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (coins.isEmpty || money < 0) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
