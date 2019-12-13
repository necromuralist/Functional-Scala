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
   */
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
