import scala.io.Source

val input = Source
  .fromFile("./Day01/input.txt")
  .getLines()
  .map(_.split("   "))
  .map { case Array(fst, snd) => (fst.toInt, snd.toInt) }
  .toList

val left = input.map(_._1)
val right = input.map(_._2)

val part1 =
  (left.sorted zip right.sorted).map((left, right) => (left - right).abs).sum

val part2 = {
  val rightOccurences = right.groupBy(identity).view.mapValues(_.size)

  left.map(x => x * rightOccurences.getOrElse(x, 0)).sum
}
