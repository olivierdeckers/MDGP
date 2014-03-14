
object VNS {

  val neighbourhoods:List[(Solution,MDGP)=>Solution] = List(NeighbourhoodStructure.insertion, NeighbourhoodStructure.swap, NeighbourhoodStructure.threeChain)

  def vns(mdgp:MDGP) = {
    var sol = MDGPSolution.greedySolution(mdgp)
    var fitness = MDGPSolution.fitness(sol, mdgp)

    var iterator = neighbourhoods.toIterator
    for(neighbourhood <- iterator) {
      println(neighbourhood.toString())
      iterator = neighbourhoods.toIterator
    }

  }
}
