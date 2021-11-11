import scala.annotation.tailrec
import scala.collection.mutable

object Chapter8 {

  //8.1 triple steps
  def countTripleSteps(n: Int): Int = {
    val cache: mutable.Map[Int, Int] = mutable.Map()

    def innerCountTripleSteps(stepsLeft: Int): Int = {
      stepsLeft match {
        case 0 => 1
        case x if x < 0 => 0
        case _ =>
          cache.getOrElseUpdate(stepsLeft, {
            val oneStep: Int = countTripleSteps(stepsLeft - 1)
            val twoSteps: Int = countTripleSteps(stepsLeft - 2)
            val threeSteps: Int = countTripleSteps(stepsLeft - 3)

            oneStep + twoSteps + threeSteps
          })
      }
    }

    innerCountTripleSteps(n)
  }

  //8.2 robot in a grid
  object RobotMove extends Enumeration {
    val DOWN, RIGHT = Value
  }

  def robotInGrid(grid: Array[Array[Boolean]], r: Int, c: Int): Seq[RobotMove.Value] = {
    val cache: mutable.Map[(Int, Int), Seq[RobotMove.Value]] = mutable.Map[(Int, Int), Seq[RobotMove.Value]]()

    def innerRobotInGrid(robotLocation: (Int, Int), currentPath: Seq[RobotMove.Value]): Seq[RobotMove.Value] = {
      robotLocation match {
        case (i, j) if i == r - 1 && j == c - 1 => currentPath
        case (i, j) if i < 0 || i >= r || j < 0 || j >= c || !grid(i)(j) => Seq[RobotMove.Value]()
        case (i, j) =>
          cache.getOrElseUpdate((i, j), {
            val pathsByMovingRight: Seq[RobotMove.Value] = innerRobotInGrid((i + 1, j), currentPath :+ RobotMove.RIGHT)
            val pathsByMovingDown: Seq[RobotMove.Value] = innerRobotInGrid((i, j + 1), currentPath :+ RobotMove.DOWN)

            Some(pathsByMovingRight).filter(_.nonEmpty).getOrElse(pathsByMovingDown)
          })
      }
    }

    innerRobotInGrid((0, 0), Seq[RobotMove.Value]())
  }

  //8.3 magic index
  def findMagicIndex(arr: Array[Int]): Option[Int] = {
    @tailrec
    def innerFindMagicIndex(start: Int, end: Int): Option[Int] = {
      if (end - start < 3) {
        (start until end).find(i => arr(i) == i)
      } else {
        val mid = (end - start) / 2
        if (arr(mid) == mid) {
          Some(mid)
        } else if (arr(mid) > mid) {
          innerFindMagicIndex(start, mid)
        } else {
          innerFindMagicIndex(mid, end)
        }
      }
    }

    innerFindMagicIndex(0, arr.length)
  }

  //8.4 power set
  def generateSubsets(s: Set[Int]): Set[Set[Int]] = {
    s.foldLeft(Set[Set[Int]](Set[Int]()))((currentSubsets, currentSetMember) => {
      currentSubsets.flatMap(subset => Set(subset, subset.+(currentSetMember)))
    }).filter(_.nonEmpty)
  }

  //8.5 recursive multiply
  def recursiveMultiply(a: Int, b: Int): Int = {
    @tailrec
    def innerRecursiveMultiply(count: Int, sum: Int = 0): Int = {
      if (count == 0) {
        sum
      } else {
        innerRecursiveMultiply(count - 1, sum + a)
      }
    }

    innerRecursiveMultiply(b)
  }

  //8.6 towers of hanoi
  def towersOfHanoi(n: Int): (mutable.Stack[Int], mutable.Stack[Int], mutable.Stack[Int]) = {
    val leftTower: mutable.Stack[Int] = mutable.Stack[Int]()
    val midTower: mutable.Stack[Int] = mutable.Stack[Int]()
    val rightTower: mutable.Stack[Int] = mutable.Stack[Int]()

    leftTower.pushAll((1 to n).reverse)

    def moveSubTower(towerHeightToMove: Int,
                     source: mutable.Stack[Int],
                     destination: mutable.Stack[Int],
                     buffer: mutable.Stack[Int]): Unit = {
      if (towerHeightToMove > 0) {
        moveSubTower(towerHeightToMove - 1, source, buffer, destination)
        destination.push(source.pop())
        moveSubTower(towerHeightToMove - 1, buffer, destination, source)
      }
    }

    moveSubTower(n, leftTower, rightTower, midTower)

    (leftTower, midTower, rightTower)
  }

  //8.7 string permutations without duplications
  def permutationsWithoutDups(str: String): Set[String] = {
    def addMemberInAllPossiblePositions(member: Int, list: Seq[Int]): Set[Seq[Int]] = {
      (0 to list.size).map(idxOfInsertion => {
        if (idxOfInsertion == 0) {
          list.prepended(member)
        } else if (idxOfInsertion == list.size) {
          list.appended(member)
        } else {
          val (before, after) = list.splitAt(idxOfInsertion)
          before ++ Seq(member) ++ after
        }
      }).toSet
    }

    def buildPermutationOfNums(nums: Seq[Int]): Set[Seq[Int]] = nums match {
      case head :: Nil => Set(Seq(head))
      case head :: tail =>
        val permutationsOfTail: Set[Seq[Int]] = buildPermutationOfNums(tail)

        permutationsOfTail.flatMap(permutation => {
          addMemberInAllPossiblePositions(head, permutation)
        })
    }

    buildPermutationOfNums((0 until str.length).toList)
      .map(indicesPermutation => indicesPermutation.map(i => str(i)).mkString)
  }

  //8.8 string permutations with duplications
  def permutationsWithDups(str: String): Set[String] = {
    val charToNumOfOccurrences: Map[Char, Int] = str.map(c => (c, 1))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sum)
      .toMap

    def buildPermutations(charsLeft: Map[Char, Int], currentPermutations: Set[String]): Set[String] = {
      if (charsLeft.isEmpty) {
        currentPermutations
      } else {
        charsLeft.map({
          case (c, numOfOccurrences) =>
            val permutationsWithCurrentChar: Set[String] = currentPermutations.map(s => s"$s$c")
            if (numOfOccurrences == 1) {
              buildPermutations(charsLeft.removed(c), permutationsWithCurrentChar)
            } else {
              buildPermutations(charsLeft + (c -> (numOfOccurrences - 1)), permutationsWithCurrentChar)
            }
        }).foldLeft(Set[String]())(_ ++ _)
      }
    }

    buildPermutations(charToNumOfOccurrences, Set(""))
  }

  //8.9 build parenthesis combinations
  def parens(n: Int): Set[String] = {
    def innerParens(parensLeft: Int, balance: Int, currentCombinations: Set[String]): Set[String] = {
      (parensLeft, balance) match {
        case (0, 0) => currentCombinations
        case (0, b) if b > 0 => currentCombinations.map(s => s"$s${(0 until b).map(_ => ")").mkString}")
        case (0, b) if b < 0 => Set[String]()
        case _ =>
          val openBrace = innerParens(parensLeft - 1, balance + 1, currentCombinations.map(s => s"$s("))
          val closeBrace = if (balance > 0) {
            innerParens(parensLeft, balance - 1, currentCombinations.map(s => s"$s)"))
          } else {
            Set[String]()
          }
          openBrace ++ closeBrace
      }
    }

    innerParens(n, 0, Set(""))
  }

  //8.10 paint fill
  type Color = (Byte, Byte, Byte)

  def fillPaint(image: Array[Array[Color]], fillLocation: (Int, Int), fillColor: Color): Unit = {
    val originalColor: Color = image(fillLocation._1)(fillLocation._2)
    val imageRows: Int = image.length
    val imageCols: Int = image.head.length

    def fillStep(currentLocation: (Int, Int)): Unit = {
      image(currentLocation._1)(currentLocation._2) = fillColor
      val possibleNextLocations: Seq[(Int, Int)] = for {
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
      } yield {
        (currentLocation._1 + dx, currentLocation._2 + dy)
      }

      val validNextLocations: Seq[(Int, Int)] = possibleNextLocations
        .filter({
          case (x, y) => x >= 0 && x < imageRows && y >= 0 && y < imageCols && image(x)(y) == originalColor
        })

      validNextLocations.foreach(loc => {
        if (image(loc._1)(loc._2) == originalColor) { // this check prevents loops
          fillStep(loc)
        }
      })
    }

    if (fillColor != originalColor) {
      fillStep(fillLocation)
    }
  }

  case class CoinsCombination(quarters: Int = 0,
                              dimes: Int = 0,
                              nickels: Int = 0,
                              pennies: Int = 0)

  //8.11 combinations of coins to make up a sum
  def coinsCombinations(n: Int): Set[CoinsCombination] = {
    val cache: mutable.Map[Int, Set[CoinsCombination]] = mutable.Map[Int, Set[CoinsCombination]]()

    def getCombinationsByUsingCoin(sumLeft: Int, coinValue: Int): Set[CoinsCombination] = {
      if (sumLeft >= coinValue) {
        cache.getOrElseUpdate(sumLeft - coinValue, innerCoinsCombinations(sumLeft - coinValue))
          .map(x => coinValue match {
            case 25 => x.copy(quarters = x.quarters + 1)
            case 10 => x.copy(dimes = x.dimes + 1)
            case 5 => x.copy(nickels = x.nickels + 1)
            case 1 => x.copy(pennies = x.pennies + 1)
          })
      } else {
        Set[CoinsCombination]()
      }
    }

    def innerCoinsCombinations(sumLeft: Int): Set[CoinsCombination] = sumLeft match {
      case 0 => Set(CoinsCombination())
      case _ =>
        getCombinationsByUsingCoin(sumLeft, 25) ++
          getCombinationsByUsingCoin(sumLeft, 10) ++
          getCombinationsByUsingCoin(sumLeft, 5) ++
          getCombinationsByUsingCoin(sumLeft, 1)
    }

    innerCoinsCombinations(n)
  }

  case class Pos(x: Int, y: Int) {
    def isOnBoard: Boolean = x >= 0 && x < 8 && y >= 0 && y < 8

    def movedBy(dx: Int, dy: Int): Option[Pos] = {
      Some(Pos(x + dx, y + dy)).filter(_.isOnBoard)
    }
  }

  //8.12
  def eightQueens(): Set[Set[Pos]] = {
    val allBoardPositions: Set[Pos] = (0 until 8).flatMap(i => {
      (0 until 8).map(j => {
        Pos(i, j)
      })
    }).toSet

    def getPositionsHitByQueen(queenLocation: Pos): Set[Pos] = {
      val posHit: mutable.Buffer[Pos] = mutable.Buffer[Pos](queenLocation)
      val directions: Seq[(Int, Int)] = Seq((0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, 1), (1, -1), (-1, -1))
      directions.foreach({
        case (dirX, dirY) =>
          var candidate: Option[Pos] = queenLocation.movedBy(dirX, dirY)
          while (candidate.nonEmpty) {
            posHit.append(candidate.get)
            candidate = candidate.flatMap(_.movedBy(dirX, dirY))
          }
      })
      posHit.toSet
    }

    val cache: mutable.Map[Set[Pos], Set[Set[Pos]]] = mutable.Map[Set[Pos], Set[Set[Pos]]]()

    def nQueens(n: Int): Set[Set[Pos]] = n match {
      case 0 => Set[Set[Pos]]()
      case 1 => allBoardPositions.map(Set(_))
      case _ =>
        val nMinusOneQueens: Set[Set[Pos]] = nQueens(n - 1)

        nMinusOneQueens.flatMap(currentPositions => {
          cache.getOrElseUpdate(currentPositions, {
            val invalidPositions: Set[Pos] = currentPositions.flatMap(getPositionsHitByQueen)
            val validPositions: Set[Pos] = allBoardPositions -- invalidPositions
            validPositions.map(newQueenPosition => currentPositions.+(newQueenPosition))
          })
        })
    }

    nQueens(8)
  }

  //8.13 stack of boxes
  case class Box(width: Int, height: Int, depth: Int) {
    def canBeStackedOn(other: Box): Boolean = {
      other.width > width &&
        other.height > height &&
        other.depth > depth
    }
  }

  def getMaxStackedBoxesHeight(boxes: Set[Box]): Int = {
    type Index = Int
    type Height = Int
    val boxesByHeightAsc: List[Box] = boxes.toList.sortBy(_.height)
    val cache: mutable.Map[(Index, Option[Box]), Height] = mutable.Map()

    def getHeightOfStackFromIndex(currentBoxIdx: Index, stackTop: Option[Box] = None): Height = {
      if (currentBoxIdx == boxesByHeightAsc.size) {
        0
      } else {
        val currentBox: Box = boxesByHeightAsc(currentBoxIdx)
        val withoutCurrent: Height = cache.getOrElseUpdate((currentBoxIdx + 1, stackTop), {
          getHeightOfStackFromIndex(currentBoxIdx + 1, stackTop)
        })

        val withCurrent: Height = if (stackTop.forall(currentTop => currentBox.canBeStackedOn(currentTop))) {
          //head can be stacked on top, we need to check both options
          currentBox.height + cache.getOrElseUpdate((currentBoxIdx + 1, Some(currentBox)), {
            getHeightOfStackFromIndex(currentBoxIdx + 1, Some(currentBox))
          })
        } else {
          0
        }
        math.max(withoutCurrent, withCurrent)
      }
    }

    getHeightOfStackFromIndex(0)
  }

  //8.14 boolean evaluation
  def countEval(expr: String, result: Boolean): Int = {
    val cache: mutable.Map[(String, Boolean), Int] = mutable.Map[(String, Boolean), Int]()

    def innerCountEval(expr: String, result: Boolean): Int = expr match {
      case "" => 0
      case "1" => if (result) 1 else 0
      case "0" => if (result) 0 else 1
      case _ =>
        val separationIndices = (1 until expr.length by 2)
        separationIndices.map(idx => {
          val operator: Char = expr(idx)
          val leftSubexpression = expr.substring(0, idx)
          val rightSubexpression = expr.substring(idx + 1, expr.length)
          lazy val leftExprFalse = cache.getOrElseUpdate((leftSubexpression, false), {
            innerCountEval(leftSubexpression, false)
          })
          lazy val leftExprTrue = cache.getOrElseUpdate((leftSubexpression, true), {
            innerCountEval(leftSubexpression, true)
          })
          lazy val rightExprFalse = cache.getOrElseUpdate((rightSubexpression, false), {
            innerCountEval(rightSubexpression, false)
          })
          lazy val rightExprTrue = cache.getOrElseUpdate((rightSubexpression, true), {
            innerCountEval(rightSubexpression, true)
          })

          (operator, result) match {
            case ('&', true) => leftExprTrue * rightExprTrue
            case ('&', false) => leftExprTrue * rightExprFalse + leftExprFalse * rightExprTrue + leftExprFalse * rightExprFalse
            case ('|', true) => leftExprTrue * rightExprFalse + leftExprFalse * rightExprTrue + leftExprTrue * rightExprTrue
            case ('|', false) => leftExprFalse * rightExprFalse
            case ('^', true) => leftExprFalse * rightExprTrue + leftExprTrue * rightExprFalse
            case ('^', false) => leftExprFalse * rightExprFalse + leftExprTrue * rightExprTrue
          }
        }).foldLeft(0)(_ + _)
    }

    innerCountEval(expr, result)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(countEval("1^0|0|1", false))
    println(countEval("0&0&0&1^1|0", true))
    println(System.currentTimeMillis() - start)
  }
}
