import scala.collection.mutable

object MuPuzzle extends App {
  type RulesApplied = List[Int]

  val rules: List[Rule] = List(Rule1, Rule2, Rule3, Rule4)

  sealed trait Rule {
    val id: Int

    def apply(word: String): Set[String]
  }

  object Rule1 extends Rule {
    val id = 1

    override def apply(word: String): Set[String] = {
      if (word.lastOption.contains('I')) {
        Set(s"${word}U")
      } else {
        Set.empty
      }
    }
  }

  object Rule2 extends Rule {
    val id = 2

    override def apply(word: String): Set[String] = {
      if (word.headOption.contains('M')) {
        Set(s"$word${word.tail}")
      } else {
        Set.empty
      }
    }
  }

  object Rule3 extends Rule {
    override val id: Int = 3

    override def apply(word: String): Set[String] = {
      val pattern = "III"
      word.zipWithIndex
        .sliding(pattern.length)
        .collect {
          case window if window.map(_._1).mkString == pattern =>
            s"${word.substring(0, window.head._2)}U${word.substring(window.last._2 + 1, word.length)}"
        }
        .toSet
    }
  }

  object Rule4 extends Rule {
    override val id: Int = 4

    override def apply(word: String): Set[String] = {
      val pattern = "UU"
      word.zipWithIndex
        .sliding(pattern.length)
        .collect {
          case window if window.map(_._1).mkString == pattern =>
            s"${word.substring(0, window.head._2)}${word.substring(window.last._2 + 1, word.length)}"
        }
        .toSet
    }
  }

  def search(start: Set[String], target: String): Option[RulesApplied] = {
    val visited: mutable.Set[String] = mutable.Set[String](start.toSeq : _*)
    val currentNodes: mutable.Queue[(String, RulesApplied)] = mutable.Queue[(String, RulesApplied)](start.toSeq.map((_, List.empty[Int])) : _*)

    var solution: Option[RulesApplied] = None

    while (solution.isEmpty && currentNodes.nonEmpty) {
      val (currentWord, rulesAppliedSoFar): (String, RulesApplied) = currentNodes.dequeue()

      val rulesAppliedForCurrent: Set[(String, Int)] = rules.flatMap { rule =>
        val next = rule(currentWord)
        next
          .diff(visited)
          .map((_, rule.id))
      }.toSet

      rulesAppliedForCurrent.foreach {
        case (`target`, ruleApplied) => solution = Some(rulesAppliedSoFar :+ ruleApplied)
        case (nextWord, ruleApplied) =>
          visited.add(nextWord)
          currentNodes.enqueue((nextWord, rulesAppliedSoFar :+ ruleApplied))
      }
    }

    solution
  }

  search(Set("MI"), "MU") match {
    case None => println("Couldn't find solution :(")
    case Some(solution) => println(s"Solution: ${solution.mkString("->")}")
  }
}
