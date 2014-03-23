import java.io.File

class MDGP(file:File) {

  val (nbElements, nbGroups, limits, distances) = initFromFile(file)

  def initFromFile(file:File) = {
    val (firstLine:String) :: distanceLines = io.Source.fromFile(file).getLines().toList
    val nbElements :: nbGroups :: groupLimits = firstLine.split(" ").map(s => toInt(s)).flatten.toList

    def readLimits(list:List[Int]) : List[(Int,Int)] = list match {
      case a :: b :: rest => (a,b) :: readLimits(rest)
      case _ => List()
    }
    val limits = readLimits(groupLimits)

    val distances = Array.ofDim[Double](nbElements, nbElements)
    distanceLines.foreach(distance => {
      val Array(a, b, d) = distance.split(" ").map(s => s.toDouble)
      distances(a.toInt)(b.toInt) = d
      distances(b.toInt)(a.toInt) = d
    })

    (nbElements, nbGroups, limits, distances)
  }

  def toInt(s: String):Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e:Exception => None
    }
  }

}

object Main extends App {

  val mdgp = new MDGP(new java.io.File("test.txt"))
  Algorithms.gvns(mdgp)

  /*val files = new java.io.File("benchmark").listFiles.filter(_.getName.endsWith(".txt"))
  val nbSamples = 20

  println("Filename\tMin\tAverage\tMax\tDuration")

  for(file <- files) {
    val mdgp = new MDGP(file)

    var sum = 0d
    var min = Double.MaxValue
    var max = 0d
    var time = 0l
    for(i <- 1 to nbSamples) {
      val start = System.currentTimeMillis()
      val sol = Algorithms.gvns(mdgp)
      time += System.currentTimeMillis() - start
      val fitness = MDGPSolution.fitness(sol, mdgp)

      sum += fitness
      min = Math.min(min, fitness)
      max = Math.max(max, fitness)
    }
    val average = sum / nbSamples.toDouble
    val averageDuration = time / nbSamples.toDouble

    println(s"${file.getName()}\t$min\t$average\t$max\t$averageDuration")
  }*/
}