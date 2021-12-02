object Problem37 {

  object Solution {

    import scala.collection.mutable

    def isRowValid(array: Array[Array[Char]], rowIdx: Int): Boolean = {
      val row = array(rowIdx)
      val uniqElements: mutable.Set[Char] = mutable.Set[Char]()
      row.foreach(x => {
        if (x != '.') {
          if (uniqElements.contains(x)) {
            return false
          } else {
            uniqElements.add(x)
          }
        }
      })
      true
    }

    def isColValid(array: Array[Array[Char]], colIdx: Int): Boolean = {
      val uniqElements: mutable.Set[Char] = mutable.Set[Char]()
      array.foreach(row => {
        val x = row(colIdx)
        if (x != '.') {
          if (uniqElements.contains(x)) {
            return false
          } else {
            uniqElements.add(x)
          }
        }
      })
      true
    }

    def isSubGridValid(array: Array[Array[Char]], row: Int, col: Int): Boolean = {
      val subGridI: Int = row / 3
      val subGridJ: Int = col / 3

      val uniqElements: mutable.Set[Int] = mutable.Set[Int]()
      for {
        i <- (3 * subGridI) to (3 * subGridI + 2)
        j <- (3 * subGridJ) to (3 * subGridJ + 2)
      } yield {
        val x = array(i)(j)
        if (x != '.') {
          if (uniqElements.contains(x)) {
            return false
          } else {
            uniqElements.add(x)
          }
        }
      }
      true
    }

    def isValidSudoku(board: Array[Array[Char]],
                      row: Int,
                      col: Int): Boolean = {
      isRowValid(board, row) &&
        isColValid(board, col) &&
        isSubGridValid(board, row, col)
    }

    def solveSudoku(board: Array[Array[Char]]): Unit = {
      val emptyFields: Array[(Int, Int)] = (for {
        i <- board.indices
        j <- board(i).indices
      } yield {
        (i, j)
      }).filter({
        case (i, j) => board(i)(j) == '.'
      }).toArray

      def innerSolveSudoku(currentIdx: Int = 0): Boolean = {
        val currEmptyFieldIdx: (Int, Int) = emptyFields(currentIdx)

        ('1' to '9').foreach(numToWrite => {
          board(currEmptyFieldIdx._1)(currEmptyFieldIdx._2) = numToWrite
          if (isValidSudoku(board, row = currEmptyFieldIdx._1, col = currEmptyFieldIdx._2)) {
            if (currentIdx < emptyFields.length - 1) {
              if (innerSolveSudoku(currentIdx + 1)) {
                return true
              }
            } else {
              //solved
              return true
            }
          }
        })

        board(currEmptyFieldIdx._1)(currEmptyFieldIdx._2) = '.'
        false
      }

      innerSolveSudoku()
    }
  }

  def main(args: Array[String]): Unit = {
    var board = Array(
      Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
    )

    var start = System.currentTimeMillis()
    Solution.solveSudoku(board)

    println(s"Solved in ${(System.currentTimeMillis() - start)}ms!")
    println(board.map(_.mkString("Array(", ", ", ")")).mkString("\n"))

    board = Array(
      Array('.', '.', '9', '7', '4', '8', '.', '.', '.'),
      Array('7', '.', '.', '.', '.', '.', '.', '.', '.'),
      Array('.', '2', '.', '1', '.', '9', '.', '.', '.'),
      Array('.', '.', '7', '.', '.', '.', '2', '4', '.'),
      Array('.', '6', '4', '.', '1', '.', '5', '9', '.'),
      Array('.', '9', '8', '.', '.', '.', '3', '.', '.'),
      Array('.', '.', '.', '8', '.', '3', '.', '2', '.'),
      Array('.', '.', '.', '.', '.', '.', '.', '.', '6'),
      Array('.', '.', '.', '2', '7', '5', '9', '.', '.'))
    start = System.currentTimeMillis()
    Solution.solveSudoku(board)

    println(s"Solved in ${(System.currentTimeMillis() - start)}ms!")
    println(board.map(_.mkString("Array(", ", ", ")")).mkString("\n"))

  }
}
