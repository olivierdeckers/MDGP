import scala.util.control.Breaks._

object Algorithms {

  def gvns(mdgp:MDGP, kmin:Int = 2, kmax:Int = 60, kstep:Int = 2, tmax:Int = 600, nrest:Int = 2, random:Boolean = true) : Solution = {
    val start = System.currentTimeMillis()

    val initialSolution:(MDGP => Solution) = if(!random) MDGPSolution.greedySolution else MDGPSolution.randomSolution

    var sol = initialSolution(mdgp)
    var fitness = MDGPSolution.fitness(sol, mdgp)
    var (sol1, fitness1) = vnd(sol, mdgp, fitness)
    sol = sol1
    fitness = fitness1
    var optSolution = sol
    var optFitness = fitness
    var k = kmin
    var niter = 0
    var itt = 0

    while(System.currentTimeMillis() - start <= tmax * 1000) {
      itt+=1
      var (newSolTemp, newFitnessTemp) = shake(sol, k, mdgp, fitness)
      var (newSol, newFitness) = vnd(newSolTemp, mdgp, newFitnessTemp)

      //MDGPSolution.fitness(newSol, mdgp)

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
            sol = initialSolution(mdgp)
            fitness = MDGPSolution.fitness(sol, mdgp)
            niter = 0
          }
          k = kmin
        }
      }
    }
    println(itt)
    optSolution
  }

  def shake(sol:Solution, k:Int, mdgp:MDGP, fitness:Double) : (Solution, Double) = {
    var result = sol
    var f = fitness
    for(_ <- 0 to k) {
      var (result1, delta) = NeighbourhoodStructure.swap(result, mdgp) //sol? elke keer hetzelfde? Moet dit niet result zijn? ;P
      result = result1
      f+=delta
      //MDGPSolution.fitness(result, mdgp)
    }
    (result, f)
  }

  def vnd(sol: Solution, mdgp: MDGP, fitness:Double) : (Solution,Double) = {
    var result : Solution = sol
    var neighbourhoods:List[(Solution,MDGP)=>(Solution, Double)] = List(NeighbourhoodStructure.insertion)
    if(mdgp.nbGroups >= 2)
      neighbourhoods = neighbourhoods ++ List[(Solution,MDGP)=>(Solution, Double)](NeighbourhoodStructure.swap)
    if(mdgp.nbGroups >= 3)
      neighbourhoods = neighbourhoods ++ List[(Solution,MDGP)=>(Solution, Double)](NeighbourhoodStructure.threeChain)

    var i = 0
    var f = fitness
    while (i<neighbourhoods.length) {

      breakable {
        for(_ <- 0 until 10) {
          val (newSol, delta) = neighbourhoods(i)(result, mdgp)

          if(delta>0) {
            f = f + delta
            result = newSol
            i = 0
            break
          }
        }
      }

      i += 1
    }

    //MDGPSolution.fitness(result, mdgp)

    (result, f)
  }
}
