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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1 else {if ( r == c ) 1 else pascal(c, r-1) + pascal(c-1, r-1) }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceAcc(chars: List[Char], unmatched: Int): Boolean = {
      if (chars.isEmpty) unmatched == 0 else{
    	  if (unmatched < 0) false else {
    		  if (chars.head == '(') balanceAcc(chars.tail, unmatched +1) else {
    		    if (chars.head == ')') balanceAcc(chars.tail, unmatched -1) else balanceAcc(chars.tail, unmatched)
    		  }
    	  }
      }
    }
    
    balanceAcc(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0 else {
	    if (money == 0) 1 else {
		    if (coins.isEmpty) 0 else {
		        val remainder = money - coins.head
		        countChange(remainder, coins) + countChange(money, coins.tail) 
		    }
	    }
  	}
  }
}
