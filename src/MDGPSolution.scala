import scala.collection.immutable.IntMap
import scala.util.Random

class Solution(val groups:IntMap[Int], val groupSizes:IntMap[Int]) {

}

object MDGPSolution {

  def greedySolution(mdgp: MDGP): Solution = {

    var nbItemsNeeded = IntMap[Int]()
    var groupSizes = IntMap[Int]()
    for(i <- 0 until mdgp.nbGroups) {
      nbItemsNeeded += i -> mdgp.limits(i)._1
      groupSizes += (i -> 0)
    }

    var sol = new Solution(IntMap[Int](), groupSizes)

    for(i <- 0 until mdgp.nbElements) {//Random.shuffle((0 until mdgp.nbElements).toList)) {
      var maxAvgDistance = -1d
      var bestGroup = -1
      for(g <- Random.shuffle((0 until mdgp.nbGroups).filter(i => nbItemsNeeded(i) > 0))) {
        val avgDistance = calcAvgDistance(i, g, mdgp, sol)
        if(avgDistance > maxAvgDistance) {
          maxAvgDistance = avgDistance
          bestGroup = g
        }
      }

      if(bestGroup == -1) {
        for(g <- 0 until mdgp.nbGroups) {
          val avgDistance = calcAvgDistance(i, g, mdgp, sol)
          if(avgDistance > maxAvgDistance) {
            maxAvgDistance = avgDistance
            bestGroup = g
          }
        }
      }

      sol = new Solution(sol.groups + (i -> bestGroup), sol.groupSizes + (bestGroup -> (sol.groupSizes(bestGroup) + 1)))
      nbItemsNeeded += bestGroup -> (nbItemsNeeded(bestGroup) - 1)
    }

    sol
  }

  def calcAvgDistance(element:Int, group:Int, mdgp:MDGP, sol:Solution) = {
    val distances = (0 until mdgp.nbElements)
      .filter(e => sol.groups.contains(e) && sol.groups(e) == group)
      .map(e => mdgp.distances(element)(e))

    if(distances.size == 0)
      0d
    else
      distances.sum / distances.size.toDouble
  }

  def fitness(sol: Solution, mdgp: MDGP) = {
    var sum = 0d
    for(i <- 0 until mdgp.nbElements) {
      for(j <- 0 until i) {
        if(sol.groups(j) == sol.groups(i))
          sum += mdgp.distances(j)(i)
      }
    }

    sum
  }
}
