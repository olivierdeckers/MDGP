import scala.util.Random

object NeighbourhoodStructure {

  val random = new Random()

  def insertion(sol:Solution, mdgp:MDGP) : Solution = {
    val elem = random.nextInt(mdgp.nbElements)
    val group = sol.groups(elem)
    val newGroup = random.nextInt(mdgp.nbGroups)

    val groupSize = sol.groupSizes(group) - 1
    val newGroupSize = sol.groupSizes(newGroup) + 1

    if(groupSize < mdgp.limits(group)._1 || newGroupSize > mdgp.limits(group)._2)
      return sol

    new Solution(sol.groups - elem + (elem -> newGroup),
      sol.groupSizes + (group -> groupSize) + (newGroup -> newGroupSize))
  }

  def swap(sol:Solution, mdgp:MDGP) : Solution = {
    if(mdgp.nbGroups <= 1) throw new IllegalArgumentException("At least 2 groups")

    var i,j = random.nextInt(mdgp.nbElements)
    while(sol.groups(i) == sol.groups(j)){
      j = random.nextInt(mdgp.nbElements)
    }

    new Solution(sol.groups - i - j + (i -> sol.groups(j)) + (j -> sol.groups(i)), sol.groupSizes)
  }

  def threeChain(sol:Solution, mdgp:MDGP) : Solution = {
    if(mdgp.nbGroups <= 2) throw new IllegalArgumentException("At least 3 groups")

    var i,j,k = random.nextInt(mdgp.nbElements)
    while(sol.groups(i) == sol.groups(j)){
      j = random.nextInt(mdgp.nbElements)
    }
    while(sol.groups(i) == sol.groups(k) || sol.groups(j) == sol.groups(k)){
      k = random.nextInt(mdgp.nbElements)
    }

    new Solution(sol.groups - i - j - k + (i -> sol.groups(j)) + (j -> sol.groups(k)) + (k -> sol.groups(i)), sol.groupSizes)
  }
}
