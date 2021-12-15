import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import java.util.regex.Pattern

object Day14 extends IOApp {
  private val ruleSplitPattern = Pattern.quote(" -> ")

  def expandRules(rules: Map[String, Char], iterations: Int): Map[String, List[Char]] = {
    rules.map { case (s, _) => (s, inductPairs(s.toList, rules, iterations - 1)) }
  }
  // Ideas instead of iterating...
  // dynamic programming? build up a mapping of greater and greater substrings instead of doing 2 at once?
  // or, just recurse once we have the new one. We can basically say "ah yes, I know that these are now adjacent, and can evaluate this subset".
  //    set an arbitrary depth
  def iterate[F[_]](template: List[Char], rules: Map[String, Char]): List[Char] = {
    val additions = template
      .sliding(2)
      .map { pair => rules(pair.mkString) }

    fs2.Stream.emits(template).interleaveAll(fs2.Stream.emits(additions.toList)).toList
  }

  private def mergeFrequencies(freqs: List[(Char, Long)]): Map[Char, Long] = freqs.groupMapReduce(_._1)(_._2)(_ + _)
  // All right so how do we recursively evaluate these rules?
  // For each pair, we say "yo, this char is inserted in this pair", so we'll count that. Then we say, please tell me the char frequencies for the pairs for this pair, and this pair
  private def evaluateRuleFrequencies(
      pair: List[Char],
      rules: Map[String, Char],
      iterationsLeft: Int,
      memo: Map[(String, Int), Map[Char, Long]] = Map.empty
  ): (Map[Char, Long], Map[(String, Int), Map[Char, Long]]) = {

    memo
      .get((pair.mkString, iterationsLeft))
      .map((_, memo))
      .getOrElse {
        val addition = rules(pair.mkString)
        if (iterationsLeft > 0) {
          // So when can I update the memory? Like, do I even have a valid thing to remember? We cut off at X iterations. Yes, that is the point.
          val (leftElaboration, leftMemo) =
            evaluateRuleFrequencies(pair.head :: addition :: Nil, rules, iterationsLeft - 1, memo)
          val (rightElaboration, rightMemo) =
            evaluateRuleFrequencies(addition :: pair.tail, rules, iterationsLeft - 1, memo ++ leftMemo)
          (
            mergeFrequencies((addition, 1L) :: leftElaboration.toList ++ rightElaboration.toList),
            leftMemo ++ rightMemo ++ memo
          )
        } else {
          val root = Map(addition -> 1L)
          (root, memo + ((pair.mkString, iterationsLeft) -> root))
        }
      }

  }

  private def inductPairs(pair: List[Char], rules: Map[String, Char], iterationsLeft: Int): List[Char] = {
    val addition = rules(pair.mkString)
    if (iterationsLeft > 0) {
      // Need to do a fusing of the subcalls (i.e. eval the rule from the left, the right)
      val leftElaboration = inductPairs(pair.head :: addition :: Nil, rules, iterationsLeft - 1)
      val rightElaboration = inductPairs(addition :: pair.tail, rules, iterationsLeft - 1)
      // ok so I have in this instance
      // (h, rules(h, addition) addition) , (addition, rules(addition, tail), tail)
      // I... think I can just add right.tail
      // given a pair, calculate this rule. Then recurs with (left, added), (right, added)
      leftElaboration ++ rightElaboration.tail
    } else {
      pair.head :: addition :: pair.tail
    }
  }

  def iterateRecurse(template: List[Char], rules: Map[String, Char], stepsLeft: Int): List[Char] = {
    template.sliding(2).map(inductPairs(_, rules, stepsLeft - 1)).toList.reduce[List[Char]] {
      case (l: List[Char], r: List[Char]) =>
        l ++ r.tail
    }
  }

  private def solve(template: List[Char], rules: Map[String, Char], targetIterations: Int): Long = {

    var accumulatedMemory = Map[(String, Int), Map[Char, Long]]()
    var allFreqs = template.map((_, 1L))

    template.sliding(2).foreach { pair =>
      val (freqs, memo) = evaluateRuleFrequencies(pair, rules, targetIterations - 1, accumulatedMemory)
      allFreqs = allFreqs ++ freqs.toList
      accumulatedMemory = accumulatedMemory ++ memo
    }
    val frequencies = mergeFrequencies(allFreqs)

    val mostCommon = frequencies.maxBy(_._2)
    val leastCommon = frequencies.minBy(_._2)
    mostCommon._2 - leastCommon._2
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      nonEmptyLines <- io.lines[IO](Path("inputs/day14.txt")).compile.toList
      template = nonEmptyLines.head.toList
      rules = nonEmptyLines.tail.map { l =>
        val parts = l.split(ruleSplitPattern)
        (parts(0), parts(1).toCharArray()(0))
      }.toMap
      part1 = solve(template, rules, 10)
      _ = println(part1)
      part2 = solve(template, rules, 40)
      _ = println(part2)

    } yield ExitCode.Success
  }
}
