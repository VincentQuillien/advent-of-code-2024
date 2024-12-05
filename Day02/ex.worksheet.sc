import scala.io.Source

val input = Source
  .fromFile("./Day02/input.txt")
  .getLines()
  .map(_.split(" ").map(_.toInt).toList)
  .toList

def getVariations(report: List[Int]) =
  report
    .sliding(2)
    .toList
    .map(pair => pair(0) - pair(1))
    .map {
      case x if 1 to 3 contains x   => 1
      case x if -3 to -1 contains x => -1
      case _                        => 0
    }

def getMaxVariationCount(variations: List[Int]) =
  Math.max(variations.count(_ == 1), variations.count(_ == -1))

val part1 = {
  input
    .map(getVariations)
    .count(variation => getMaxVariationCount(variation) == variation.length)
}

val part2 = {
  def checkDampenedReport(report: List[Int]) = {
    (0 until report.length).exists((index) => {
      val subReport =
        report.slice(0, index) ++ report.slice(index + 1, report.length)
      val variations = getVariations(subReport)
      getMaxVariationCount(variations) == variations.length
    })
  }
  (input zip input
    .map(getVariations))
    .count { (report, variations) =>
      val count = getMaxVariationCount(variations)
      count == variations.length || (count >= (variations.length - 2) && checkDampenedReport(
        report
      ))
    }
}
