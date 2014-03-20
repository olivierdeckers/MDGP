import scala.util.control.Breaks._

object VNS {

  def vns(mdgp:MDGP) : Solution = {
    var neighbourhoods:List[(Solution,MDGP)=>Solution] = List(NeighbourhoodStructure.insertion)
    if(mdgp.nbGroups >= 2)
      neighbourhoods = neighbourhoods ++ List[(Solution,MDGP)=>Solution](NeighbourhoodStructure.swap)
    //if(mdgp.nbGroups >= 3)
    //  neighbourhoods = neighbourhoods ++ List[(Solution,MDGP)=>Solution](NeighbourhoodStructure.threeChain)

    var sol = MDGPSolution.greedySolution(mdgp)

    var i = 0
    while (i<neighbourhoods.length) {
      val f = MDGPSolution.fitness(sol, mdgp)

      breakable {
        for(_ <- 0 until 10) {
          val newSol = neighbourhoods(i)(sol, mdgp)
          val newF = MDGPSolution.fitness(newSol, mdgp)

          if(newF > f) {
            sol = newSol
            i = 0
            break
          }
        }
      }

      i += 1
    }

    return sol
  }
}
