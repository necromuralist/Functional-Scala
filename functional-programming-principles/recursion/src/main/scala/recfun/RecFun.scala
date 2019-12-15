package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
    * Exercise 1
    * Calculates values in Pascal's Triangle
    * 
    * @param c The column number (0-based)
    * @param r The row number (0-based)
    * @return Element of Pascal's Triangle at column c and row r
    */
  def pascal(c: Int, r: Int): Int = {
    @annotation.tailrec
    def pascal_accumulated(column: Int, row: Int, accumulator: Int): Int = column match{
      case 0 => {
        require(column <= row)
        accumulator
      }
      case _ => {
        require(column <= row)
        require(column >= 0)
        pascal_accumulated(column - 1, row, accumulator * (row + 1 - column)/column)
      }
    } // pascal_accumulated
    pascal_accumulated(c, r, 1)
  } // pascal

  /**
    * Exercise 2
    * Checks if a string contains a balanced set of parentheses
    * @param chars list of characters to check
    */
  def balance(chars: List[Char]): Boolean = {
    @annotation.tailrec
    def balance_accumulated(chars: List[Char], lefts: Int, rights: Int): Boolean = chars match {
      case Nil => lefts == rights
      case head :: tail => {
        val left = if (head == '(') 1 else 0
        val right = if (head == ')') 1 else 0
        if (right + rights > left + lefts)
          false
        else
          balance_accumulated(tail, lefts + left, rights + right)

      }
    } // balance_accumulated
    balance_accumulated(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0)
      0
   else if (money == 0)
      1
    else 
    coins match {
      case Nil => 0
      case head :: tail => countChange(money - head, head :: tail) + countChange(money, tail)
    }
  }
}
