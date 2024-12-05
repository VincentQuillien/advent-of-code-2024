import scala.io.Source

val input = Source
  .fromFile("./Day05/input.txt")
  .getLines()
  .toList

val (inputOrderings, inputUpdates) = input.splitAt(input.indexOf(""))

val orderMap = inputOrderings
  .map(_.split('|').toList)
  .groupMap(_.head)(_.last)
  .view
  .mapValues(_.toSet)

val updates = inputUpdates.tail.map(_.split(',').toList)

def sortUpdate(update: List[String]) =
  update.sortWith((a, b) => orderMap.contains(a) && orderMap(a).contains(b))

def getMiddle(update: List[String]) =
  update((update.length - 1) / 2).toInt

val part1 = updates
  .filter(update => update == sortUpdate(update))
  .map(getMiddle)
  .sum

val part2 = updates
  .map(update => (update, sortUpdate(update)))
  .filter((update, sorted) => update != sorted)
  .map((_, sorted) => getMiddle(sorted))
  .sum
