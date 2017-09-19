package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance(":-)".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
        if( c == 0 || c >= r)
        1
      else
        pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def help(flag: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty)
          return flag == 0
        else {
          if (chars.head == '(')
            return help(flag + 1, chars.tail)

          if (chars.head == ')')
            return if(flag == 0) false else help(flag - 1, chars.tail)

            return help(flag, chars.tail)
        }
      }
      help(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty)
        0
      else{
        if(money == 0)
          1
        else
          countChange(money - coins.head,coins ) + countChange(money, coins.tail)
      }
    }
  }
