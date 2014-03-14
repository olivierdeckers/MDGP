
class MDGP(file:String) {

  val (nbElements, nbGroups, limits, distances) = initFromFile(file)

  def initFromFile(file:String) = {
    val (firstLine:String) :: distanceLines = io.Source.fromFile(file).getLines().toList
    val nbElements :: nbGroups :: groupLimits = firstLine.split(" ").map(s => toInt(s)).flatten.toList

    def readLimits(list:List[Int]) : List[(Int,Int)] = list match {
      case a :: b :: rest => (a,b) :: readLimits(rest)
      case _ => List()
    }
    val limits = readLimits(groupLimits)

    val distances = Array.ofDim[Int](nbElements, nbElements)
    distanceLines.foreach(distance => {
      val Array(a, b, d) = distance.split(" ").map(s => s.toInt)
      distances(a)(b) = d
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
  val mdgp = new MDGP("test.txt")//"RanInt_n010_ds_01.txt")

  val sol = VNS.vns(mdgp)

  println(MDGPSolution.fitness(sol, mdgp))
//  val sol = MDGPSolution.greedySolution(mdgp)
//
//  val fitness = MDGPSolution.fitness(sol, mdgp)
//
//  println(fitness)
}