
object VNS {

  val neighbourhoods:List[(Solution,MDGP)=>Solution] = List(NeighbourhoodStructure.insertion, NeighbourhoodStructure.swap)//, NeighbourhoodStructure.threeChain)

  def vns(mdgp:MDGP) : Solution = {
    var sol = MDGPSolution.greedySolution(mdgp)
    var fitness = MDGPSolution.fitness(sol, mdgp)

    val solutions = Iterator.iterate(sol) { (sol => {
      val f = MDGPSolution.fitness(sol, mdgp)

      for(neighbourhood <- neighbourhoods) {
        for(i <- 0 until 10) {
          val newSol = neighbourhood(sol, mdgp)
          val newF = MDGPSolution.fitness(newSol, mdgp)

          if(newF > f) {
           return newSol
          }
        }
      }

      return null
    }) : (Solution => Solution)}

    val trace = solutions.takeWhile(sol => sol != null).toList

    return (sol :: trace).last

  }
}
