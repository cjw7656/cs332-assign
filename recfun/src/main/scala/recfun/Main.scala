package recfun
import common._
import scala.annotation.tailrec


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
		if (c == r || c == 0) 1
		else pascal(c - 1, r - 1) + pascal(c, r - 1)
	}

	/**
	 * Exercise 2
	 */
	def balance(chars: List[Char]): Boolean = {
		@tailrec
		def balance_chk(chars: List[Char], count: Int): Boolean = {
			if (chars.isEmpty) count == 0
			else if (count < 0) false
			else if (chars.head == '(') balance_chk(chars.tail, count + 1)
			else if (chars.head == ')') balance_chk(chars.tail, count - 1)
			else balance_chk(chars.tail, count)
		}

		balance_chk(chars, 0)

	}

	def countChange(money: Int, coins: List[Int]): Int = {
		if (money == 0) 1
		else if (coins.isEmpty) 0
		else if (money >= coins.head) countChange(money, coins.tail) + countChange(money - coins.head, coins)
		else countChange(money, coins.tail)
	}
}











