object Problem36 {

  object Solution {

    def areRowsValid(array: Array[Array[Char]]): Boolean = {
      array.forall(row => {
        val numsInRow = row.filterNot(_ == '.')
        numsInRow.length == numsInRow.toSet.size
      })
    }

    def areColsValid(array: Array[Array[Char]]): Boolean = {
      (0 until 9).forall(colNum => {
        val numsInCol = array.map(_(colNum)).filterNot(_ == '.')
        numsInCol.length == numsInCol.toSet.size
      })
    }

    def areSubGridsValid(array: Array[Array[Char]]): Boolean = {
      (for{
        subGridI <- (0 to 2)
        subGridJ <- (0 to 2)
      }yield{
        val numsInSubGrid: Array[Char] = (for{
          i <- (3*subGridI to 3*subGridI + 2)
          j <- (3*subGridJ to 3*subGridJ + 2)
        }yield{
          array(i)(j)
        }).filterNot(_ == '.').toArray

        numsInSubGrid.length == numsInSubGrid.toSet.size
      }).reduce(_ && _)
    }

    def isValidSudoku(board: Array[Array[Char]]): Boolean = {
      areRowsValid(board) &&
        areColsValid(board) &&
        areSubGridsValid(board)
    }
  }

  def main(args: Array[String]): Unit = {
  }
}
