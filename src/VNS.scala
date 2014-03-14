import scala.util.control.Breaks._

object VNS {

  val neighbourhoods:List[(Solution,MDGP)=>Solution] = List(NeighbourhoodStructure.insertion, NeighbourhoodStructure.swap)//, NeighbourhoodStructure.threeChain)

  def vns(mdgp:MDGP) : Solution = {
    var sol = MDGPSolution.greedySolution(mdgp)

    var i = 0
    while (i<neighbourhoods.length) {
      val f = MDGPSolution.fitness(sol, mdgp)

      for(_ <- 0 until 10) {
        val newSol = neighbourhoods(i)(sol, mdgp)
        val newF = MDGPSolution.fitness(newSol, mdgp)

        if(newF > f) {
          sol = newSol
          i = 0
          break
        }
      }

      i += 1
    }

    return sol
  }
}
