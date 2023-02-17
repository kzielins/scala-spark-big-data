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
    if (c==0 || c==r) 1
    else pascal(c,r-1)+pascal(c-1,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def inBalance(chars: List[Char],count : Int): Int = {
      if (chars.isEmpty) count
      else if (count<0) count //prevent sub zero "counting not enoght"
      else if (chars.head == '(') inBalance(chars.tail,count+1)
      else if (chars.head == ')') inBalance(chars.tail,count-1)
      else inBalance(chars.tail,count)
    }
    inBalance(chars,0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1 //success
    else if (coins.isEmpty || money < 0) 0 //
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
