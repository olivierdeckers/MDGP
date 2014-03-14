import scala.util.Random

object NeighbourhoodStructure {

  val random = new Random()

  def insertion(sol:Solution, mdgp:MDGP) : Solution = {
    val elem = random.nextInt(mdgp.nbElements)
    val group = sol.groups(elem)
    val newGroup = random.nextInt(mdgp.nbElements)

    val groupSize = sol.groupSizes(group) - 1
    val newGroupSize = sol.groupSizes(newGroup) + 1

    if(groupSize < mdgp.limits(group)._1 || newGroupSize > mdgp.limits(group)._2)
      return sol

    new Solution(sol.groups - elem + (elem -> newGroup),
      sol.groupSizes + (group -> groupSize) + (newGroup -> newGroupSize))
  }

  def swap(sol:Solution, mdgp:MDGP) = {

  }

  def threeChain(sol:Solution, mdgp:MDGP) = {

  }
}
