import scala.util.Random

object NeighbourhoodStructure {

  val random = new Random()

  def insertion(sol:Solution, mdgp:MDGP) : (Solution, Double) = {
    val elem = random.nextInt(mdgp.nbElements)
    val group = sol.groups(elem)
    val newGroup = random.nextInt(mdgp.nbGroups)

    val groupSize = sol.groupSizes(group) - 1
    val newGroupSize = sol.groupSizes(newGroup) + 1

    if(groupSize < mdgp.limits(group)._1 || newGroupSize > mdgp.limits(group)._2)
      return (sol,0)

    val delta = sol.sumOfDiversities(elem)(newGroup)-sol.sumOfDiversities(elem)(group)

    var sumOfDiv = sol.sumOfDiversities
    for(j<- 0 until mdgp.nbElements){
      var column:List[Double] = sumOfDiv(j)
      column = column.updated(group, column(group)-mdgp.distances(elem)(j))
      column = column.updated(newGroup, column(newGroup)+mdgp.distances(elem)(j))
      sumOfDiv = sumOfDiv.updated(j, column)
    }

    (new Solution(sol.groups - elem + (elem -> newGroup),
      sol.groupSizes + (group -> groupSize) + (newGroup -> newGroupSize), sumOfDiv), delta)
  }

  def swap(sol:Solution, mdgp:MDGP) : (Solution, Double) = {
    if(mdgp.nbGroups <= 1) throw new IllegalArgumentException("At least 2 groups")

    var i,j = random.nextInt(mdgp.nbElements)
    while(sol.groups(i) == sol.groups(j)){
      j = random.nextInt(mdgp.nbElements)
    }

    val delta =
      sol.sumOfDiversities(i)(sol.groups(j))-sol.sumOfDiversities(i)(sol.groups(i)) +
      sol.sumOfDiversities(j)(sol.groups(i))-sol.sumOfDiversities(j)(sol.groups(j)) -
      2*mdgp.distances(i)(j)

    var sumOfDiv = sol.sumOfDiversities
    for(l<- 0 until mdgp.nbElements){
      var column:List[Double] = sumOfDiv(l)
      column = column.updated(sol.groups(j), column(sol.groups(j))-mdgp.distances(l)(j)+mdgp.distances(l)(i))
      column = column.updated(sol.groups(i), column(sol.groups(i))+mdgp.distances(l)(j)-mdgp.distances(l)(i))
      sumOfDiv = sumOfDiv.updated(l, column)
    }

    (new Solution(sol.groups - i - j + (i -> sol.groups(j)) + (j -> sol.groups(i)), sol.groupSizes, sumOfDiv), delta)
  }

  def threeChain(sol:Solution, mdgp:MDGP) : (Solution, Double) = {
    if(mdgp.nbGroups <= 2) throw new IllegalArgumentException("At least 3 groups")

    var i,j,k = random.nextInt(mdgp.nbElements)
    while(sol.groups(i) == sol.groups(j)){
      j = random.nextInt(mdgp.nbElements)
    }
    while(sol.groups(i) == sol.groups(k) || sol.groups(j) == sol.groups(k)){
      k = random.nextInt(mdgp.nbElements)
    }

    val delta =
      sol.sumOfDiversities(i)(sol.groups(j))-sol.sumOfDiversities(i)(sol.groups(i)) +
      sol.sumOfDiversities(j)(sol.groups(k))-sol.sumOfDiversities(j)(sol.groups(j)) +
      sol.sumOfDiversities(k)(sol.groups(i))-sol.sumOfDiversities(k)(sol.groups(k)) -
      mdgp.distances(i)(j) - mdgp.distances(j)(k)-mdgp.distances(k)(i)

    var sumOfDiv = sol.sumOfDiversities
    for(l<- 0 until mdgp.nbElements){
      var column:List[Double] = sumOfDiv(l)
      column = column.updated(sol.groups(j), column(sol.groups(j))+mdgp.distances(l)(i)-mdgp.distances(l)(j))
      column = column.updated(sol.groups(k), column(sol.groups(k))+mdgp.distances(l)(j)-mdgp.distances(l)(k))
      column = column.updated(sol.groups(i), column(sol.groups(k))+mdgp.distances(l)(k)-mdgp.distances(l)(i))
      sumOfDiv = sumOfDiv.updated(l, column)
    }

    (new Solution(sol.groups - i - j - k + (i -> sol.groups(j)) + (j -> sol.groups(k)) + (k -> sol.groups(i)), sol.groupSizes, sumOfDiv), delta)
  }
}
