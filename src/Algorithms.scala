import scala.util.control.Breaks._

object Algorithms {

  def gvns(mdgp:MDGP, kmin:Int = 2, kmax:Int = 60, kstep:Int = 2, tmax:Int = 600, nrest:Int = 2) : Solution = {
    val start = System.currentTimeMillis()

    var sol = MDGPSolution.greedySolution(mdgp)
    sol = vnd(sol, mdgp)
    var fitness = MDGPSolution.fitness(sol, mdgp)
    var optSolution = sol
    var optFitness = fitness
    var k = kmin
    var niter = 0

    while(System.currentTimeMillis() - start <= tmax * 1000) {
      var newSol = shake(sol, k, mdgp)
      newSol = vnd(sol, mdgp)
      val newFitness = MDGPSolution.fitness(newSol, mdgp)

      if(newFitness > fitness) {
        sol = newSol
        fitness = newFitness
        k = kmin
      }
      else {
        k = k + kstep
        if (k>kmax) {
          niter += 1
        }
        if(niter == nrest) {
          if(fitness > optFitness) {
            optSolution = sol
            optFitness = fitness
            sol = MDGPSolution.greedySolution(mdgp)
            niter = 0
          }
          k = kmin
        }
      }
    }

    optSolution
  }

  def shake(sol:Solution, k:Int, mdgp:MDGP) : Solution = {
    var result = sol
    for(_ <- 0 to k) {
      result = NeighbourhoodStructure.swap(sol, mdgp)
    }
    result
  }

  def vnd(sol: Solution, mdgp: MDGP) : Solution = {
    var result : Solution = sol
    var neighbourhoods:List[(Solution,MDGP)=>Solution] = List(NeighbourhoodStructure.insertion)
    if(mdgp.nbGroups >= 2)
      neighbourhoods = neighbourhoods ++ List[(Solution,MDGP)=>Solution](NeighbourhoodStructure.swap)
    //if(mdgp.nbGroups >= 3)
    //  neighbourhoods = neighbourhoods ++ List[(Solution,MDGP)=>Solution](NeighbourhoodStructure.threeChain)

    var i = 0
    while (i<neighbourhoods.length) {
      val f = MDGPSolution.fitness(result, mdgp)

      breakable {
        for(_ <- 0 until 10) {
          val newSol = neighbourhoods(i)(result, mdgp)
          val newF = MDGPSolution.fitness(newSol, mdgp)

          if(newF > f) {
            result = newSol
            i = 0
            break
          }
        }
      }

      i += 1
    }

    result
  }
}
