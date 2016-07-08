package recfun

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }
    println(balance(augmentString("none").toList))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def factorial(x: Int): Int = {
      def factorialAux(current: Int, total: Int): Int = {
        if (current == 0) {
          total
        } else {
          factorialAux(current - 1, current * total)
        }
      }
      factorialAux(x, 1)
    }
    def line(currentLine: Int, finalLine: Int): Int = {
      if (currentLine == finalLine) {
        def cell(currentRow: Int, finalRow: Int): Int = {
          if (currentRow <= finalRow) {
            val division = factorial(currentRow) * factorial(currentLine - currentRow)
            if (division > 0) {
              val currentCell = factorial(currentLine) / division
              if (r == finalLine && c == currentRow) {
                currentCell
              } else {
                cell(currentRow + 1, finalRow)
              }
            } else {
              0
            }
          } else {
            0
          }
        }
        cell(0, currentLine)
      } else {
        line(currentLine + 1, finalLine)
      }
    }
    if (r == 0) {
      1
    }
    line(1, r)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def bucket(currentlyOpened: Int, rest: List[Char]): Boolean = {
      if (rest.isEmpty) {
        if (currentlyOpened == 0) {
          true
        } else {
          false
        }
      } else {
        if (rest.head == '(') {
          bucket(currentlyOpened + 1, rest.tail)
        } else if (rest.head == ')' && currentlyOpened > 0) {
          bucket(currentlyOpened - 1, rest.tail)
        } else {
          bucket(currentlyOpened, rest.tail)
        }
      }
    }
    if (chars.isEmpty) {
      false
    } else {
      if (chars.head != '(') {
        balance(chars.tail)
      } else {
        bucket(1, chars.tail)
      }
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def dondur(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < coins.head) {
        // 100 < 500
        0
      } else if (money == coins.head) {
        // 100 == 100
        1
      } else {
        // 100 - 10   [20,50,100,200,500]
        countChange(money - coins.head, coins) +
          // 100    [20,50,100,200,500]
          countChange(money, coins.tail)
      }
    }
    dondur(money, coins.sorted)
  }

}
