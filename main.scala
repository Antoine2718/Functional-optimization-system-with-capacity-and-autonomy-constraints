import scala.annotation.tailrec
import scala.util.Random

// Modèle du domaine
case class Location(x: Double, y: Double) {
  def distance(that: Location): Double =
    math.hypot(x - that.x, y - that.y)
}

case class Customer(id: Int, demand: Int, loc: Location)
case class Vehicle(id: Int, capacity: Int, maxDistance: Double)

// Une tournée affectée à un véhicule
case class Route(vehicle: Vehicle, customers: List[Customer]) {
  private val depot = Location(0, 0)

  lazy val load: Int = customers.map(_.demand).sum

  lazy val distance: Double = {
    val seq = depot :: customers.map(_.loc) ::: depot :: Nil
    seq.sliding(2).map { case a :: b :: Nil => a.distance(b) }.sum
  }

  def isFeasible: Boolean =
    load <= vehicle.capacity && distance <= vehicle.maxDistance

  // Inversion du segment [i,j] pour 2-Opt
  def twoOptSwap(i: Int, j: Int): Route = {
    val (pre, rest) = customers.splitAt(i)
    val (mid, post) = rest.splitAt(j - i + 1)
    Route(vehicle, pre ::: mid.reverse ::: post)
  }
}

// Solution constituée de plusieurs tournées
case class Solution(routes: List[Route]) {
  lazy val totalDistance: Double = routes.map(_.distance).sum
  lazy val isFeasible: Boolean    = routes.forall(_.isFeasible)
}

// Constructeur de solution initiale (insertion gloutonne)
object Initializer {
  def greedy(customers: List[Customer], vehicles: List[Vehicle]): Solution = {
    @tailrec
    def assign(cs: List[Customer], rs: List[Route]): List[Route] = cs match {
      case Nil => rs
      case h :: t =>
        rs.indexWhere(r => Route(r.vehicle, r.customers :+ h).isFeasible) match {
          case idx if idx >= 0 =>
            val r = rs(idx)
            assign(t, rs.updated(idx, r.copy(customers = r.customers :+ h)))
          case _ =>
            // Nouveau véhicule
            val v = vehicles(rs.size % vehicles.size)
            assign(t, rs :+ Route(v, List(h)))
        }
    }

    Solution(assign(customers, Nil))
  }
}

// Amélioration intra-tournée par 2-Opt
object TwoOpt {
  def improve(route: Route): Route = {
    val n = route.customers.length
    val best = (for {
      i <- 0 until n - 1
      j <- i + 1 until n
      candidate = route.twoOptSwap(i, j)
      if candidate.isFeasible
    } yield candidate).minByOption(_.distance)
    best.getOrElse(route)
  }

  def apply(sol: Solution): Solution =
    Solution(sol.routes.map(improve))
}

// Mouvements pour Tabu Search
sealed trait Move
case class Relocate(r1: Int, r2: Int, cIdx: Int, pos: Int) extends Move
case class Swap(r1: Int, c1: Int, r2: Int, c2: Int)     extends Move

// Tabu Search avec Relocate & Swap
object TabuSearch {
  case class Config(maxIter: Int, tabuTenure: Int)

  def search(initial: Solution, config: Config): Solution = {
    var current = initial
    var best    = initial
    var tabuList = List.empty[Move]

    for (_ <- 1 to config.maxIter) {
      // Générer voisins
      val neighbors = for {
        (r1, i) <- current.routes.zipWithIndex
        (r2, j) <- current.routes.zipWithIndex
        if i != j
        // Relocate
        cIdx   <- r1.customers.indices
        pos    <- 0 to r2.customers.length
        solRel = tryRelocate(current, i, j, cIdx, pos)
        if solRel.isFeasible
        moveRel = Relocate(i, j, cIdx, pos)
        // Swap
        c2Idx  <- r2.customers.indices
        solSwp = trySwap(current, i, cIdx, j, c2Idx)
        if solSwp.isFeasible
        moveSwp = Swap(i, cIdx, j, c2Idx)
      } yield Seq((solRel, moveRel), (solSwp, moveSwp))
      val flatNb = neighbors.flatten

      // Sélection du meilleur admissible
      val candidateOpt = flatNb
        .filterNot { case (_, m) => tabuList.contains(m) }
        .sortBy(_._1.totalDistance)
        .headOption

      candidateOpt.foreach { case (sol, move) =>
        current = sol
        tabuList = (move :: tabuList).take(config.tabuTenure)
        if (sol.totalDistance < best.totalDistance) best = sol
      }
    }
    best
  }

  private def tryRelocate(sol: Solution, r1: Int, r2: Int, cIdx: Int, pos: Int): Solution = {
    val routes = sol.routes.toArray
    val cust   = routes(r1).customers(cIdx)
    val newR1  = routes(r1).copy(customers = routes(r1).customers.patch(cIdx, Nil, 1))
    val newR2  = routes(r2).copy(customers = routes(r2).customers.patch(pos, Seq(cust), 0))
    Solution(routes.updated(r1, newR1).updated(r2, newR2).toList)
  }

  private def trySwap(sol: Solution, r1: Int, c1: Int, r2: Int, c2: Int): Solution = {
    val routes = sol.routes.toArray
    val cust1  = routes(r1).customers(c1)
    val cust2  = routes(r2).customers(c2)
    val newR1  = routes(r1).copy(customers = routes(r1).customers.updated(c1, cust2))
    val newR2  = routes(r2).copy(customers = routes(r2).customers.updated(c2, cust1))
    Solution(routes.updated(r1, newR1).updated(r2, newR2).toList)
  }
}

// Exemple d’exécution
object VRPApp extends App {
  val customers = (1 to 50).map { i =>
    Customer(i, demand = Random.nextInt(10) + 1, Location(Random.nextDouble()*100, Random.nextDouble()*100))
  }.toList

  val vehicles = (1 to 5).map(id => Vehicle(id, capacity = 100, maxDistance = 300.0)).toList

  // 1. Solution initiale
  val initSol = Initializer.greedy(customers, vehicles)
  println(s"Initial total distance: ${initSol.totalDistance}")

  // 2. Amélioration 2-Opt intra-tournée
  val twoOptSol = TwoOpt(initSol)
  println(s"After 2-Opt distance:    ${twoOptSol.totalDistance}")

  // 3. Tabu Search
  val tsConfig = TabuSearch.Config(maxIter = 500, tabuTenure = 50)
  val bestSol  = TabuSearch.search(twoOptSol, tsConfig)
  println(s"Tabu Search best dist:   ${bestSol.totalDistance}")
}
