package org.warthog.pl.knowledgecompilation.dnnf

import collection.immutable.HashMap


class Graph[T](val vertices: Set[T], val edges: Set[(T, T)]) {

  // double edges might be dangerous!!!

  /*
  def this(vs: Set[T], es: Set[(T, T)]) =
    // Since the graph is indirected, we don't want to have double edges like {(e1,e2),(e2,e1)}
    this(vs, es.foldRight(Set[(T, T)]())
                    ((e, res) => if (res.contains((e._2, e._1))) res else res + e))
  */

  def subGraph(vertices2: Set[T]) =
    new Graph[T](vertices.filter(vertices2.contains(_)),
      edges.filter(e => vertices2.contains(e._1) && vertices2.contains(e._2)))

  def \(vertices2: Set[T]) = subGraph(vertices -- vertices2)

  def union(graph: Graph[T]) = new Graph[T](this.vertices union graph.vertices, this.edges union graph.edges)

  /**
   * Number of non edges of the graph (= number of possible edges - actual edges)
   */
  lazy val fill = vertices.size * (vertices.size-1) / 2 - edges.size

  def getNeighbors(vertex: T): Set[T] =
    edges.filter(e => e._1 == vertex || e._2 == vertex).
      map(e => if (e._1 == vertex) e._2 else e._1)

  def getNeighbors(vs: Set[T]): Set[T] = vs.flatMap(getNeighbors(_)) -- vs

  def connectedComponents(vs: Set[T]): Set[Set[T]] = {
    if (vs.isEmpty)
      Set[Set[T]]()
    else {
      val c = component(vs.head)
      connectedComponents(vs -- c) + c
    }
  }

  def component(vertex: T) = {
    def getComp(current: Set[T], todo: Set[T]): Set[T] =
      if (todo.isEmpty)
        current
      else {
        val neighbors = getNeighbors(todo)
        getComp(current ++ neighbors, neighbors -- current)
      }

    getComp(Set(vertex), Set(vertex))
  }

  def fullComponents(vs: Set[T], associatedTo: Set[T]) =
    connectedComponents(vs).filter(getNeighbors(_) == associatedTo)

  /**
   * Algorithm according to "Generating all the minimal separators of a graph"
   * (1999; Anne Berry, Jean-Paul Bordat, Olivier Cogis)
   * @return A set of all minimal separators
   */
  lazy val allMinSep: Set[Set[T]] = {
    var S = Set[Set[T]]()

    for (v <- vertices) {
      val invGraph = subGraph(vertices -- (getNeighbors(v) + v))
      for (c <- invGraph.connectedComponents(invGraph.vertices)) {
        S += getNeighbors(c)
      }
    }
    var T = Set[Set[T]]()
    while (!(S -- T).isEmpty) {
      val s = (S -- T).head
      for (x <- s) {
        val invGraph = subGraph(vertices -- (s ++ getNeighbors(x)))
        S += invGraph.connectedComponents(invGraph.vertices).map(getNeighbors(_)).foldLeft(Set[T]())(_ union _)
      }
      T += s
    }

    S
  }

  /**
   * Algorithm according to "Listing all potential maximal cliques of a graph"
   * (2000; Vincent Bouchitté, Ioan Todinca)
   * @return All potential maximal cliques
   */
  def allPotMaxCliques: Set[Set[T]] = {
    def oneMoreVertex(oldG: Graph[T], newG: Graph[T], vertex: T, cliquesOld: Set[Set[T]]) = {
      var cliquesNew = Set[Set[T]]()
      for (c <- cliquesOld)
        if (Graph.isPotentialMaximalClique(c, newG))
          cliquesNew += c
        else {
          val cExtended = c + vertex
          if (Graph.isPotentialMaximalClique(cExtended, newG))
            cliquesNew += cExtended
        }

      val newGSeps = newG.allMinSep
      for (s <- newGSeps) {
        if (Graph.isPotentialMaximalClique(s + vertex, newG))
          cliquesNew += s + vertex
        if (!s.contains(vertex) && !oldG.allMinSep.contains(s))
          for (t <- newGSeps)
            for (c <- newG.fullComponents(s, s)) {// arguments s,s correct??
              val newClique = s union (t intersect c)
              if (Graph.isPotentialMaximalClique(newClique, newG))
                cliquesNew += newClique
            }
      }
      cliquesNew
    }

    val vertexList = vertices.toSeq
    var currentVs: Set[T] = null
    var currentGraph: Graph[T] = null
    var nextVs: Set[T] = Set(vertexList(0))
    var nextGraph: Graph[T] = subGraph(nextVs)

    var currentCliques = Set(Set(vertexList(0)))
    for (i <- 1 until vertexList.size - 1) {
      val v = vertexList(i)
      currentVs = nextVs
      currentGraph = nextGraph
      nextVs += v
      nextGraph = subGraph(nextVs)

      currentCliques = oneMoreVertex(currentGraph, nextGraph, v, currentCliques)
    }

    currentCliques
  }

  def allFullBlocks = {
    val blocks = for {
      s <- allMinSep
      g <- List(this \ s)
      c <- g.vertices
    } yield new Block[T](s, g.component(c), g)

    blocks//.filter(_.isFull)
  }

  def fullBlocks(associatedTo: Set[T]) =
    allFullBlocks.filter(_.component == associatedTo)
}


/**
 * Representation of a block (S,C)
 * @param minSep (S) the minimal Separator
 * @param component (C) a component from C(S)
 * @param graph The subgraph of the graph (S,C) is drawn from, only containing nodes in minSep and component!
 */
case class Block[T](val minSep: Set[T], val component: Set[T], val graph: Graph[T]) {

  lazy val vertices = minSep union component

  lazy val isFull = graph.getNeighbors(component) == minSep

  lazy val realization: Graph[T] = graph union Graph.clique(minSep)

  override def toString = "\nBlock([" + minSep.mkString(",") + "],[" + component.mkString(",") + "])"

  /* We assume that graph is always the same */
  override def equals(obj: Any) = obj match {
    case Block(m, c, _) => minSep == m && component == c
    case _ => false
  }

  override def hashCode = 17 * minSep.## + 43 * component.##

  //def this()

}

object Graph {

  def clique[T](vertices: Set[T]) =
    new Graph[T](vertices, for (i <- vertices; j <- vertices if i != j) yield (i,j))

  def isPotentialMaximalClique[T](clique: Set[T], graph: Graph[T]) = true // still to implement!!!!

}

object MinFillIn {

  val testGraph1 = new Graph(1.to(10).toSet, Set((1,2),(1,5),(1,8),(1,9),(2,3),(2,4),(2,8),(2,10),(3,4),(3,6),(3,7),(3,8),(4,7),(4,10),(5,6),(5,8),(5,10),(6,8),(6,9),(7,10),(8,10)))
  val testGraph2 = new Graph(1.to(5).toSet, Set((1,2),(2,3),(3,4)))
  val testGraph3 = new Graph(1.to(10).toSet, Set((1,5),(2,8),(2,10),(3,4),(3,7),(4,7),(6,8),(6,9),(8,10)))
  val testGraph4 = new Graph(1.to(4).toSet, Set((1,2),(1,3),(1,4)))
  val testGraph5 = new Graph(1.to(4).toSet, Set[(Int,Int)]())
  val testGraph6 = new Graph(1.to(4).toSet, Set((1,2),(1,3),(1,4),(2,3),(2,4),(3,4)))
  val testGraph7 = new Graph(1.to(4).toSet, Set((1,2),(1,3),(1,4),(2,3),(2,4)))

  def fill[T](g: Graph[T]) = {
    var mfi = HashMap[Block[T], Option[Int]]()

    val blocks = g.allFullBlocks.toSeq.sortBy(_.vertices.size)
    println(blocks)
    val cliques = g.allPotMaxCliques
    //val cliquesWithAssiciatedBlocks = cliques.map(c => g.fullComponents(c, c))

    import scala.language.implicitConversions
    implicit def optionToInt(x: Option[Int]) = x.getOrElse(Int.MaxValue)
    def optionAdd(a: Option[Int], b: Option[Int]) = if (a == None || b == None) None else Some(a.get + b.get)
    def blockInclusionMinimal(b: Block[T]) = !blocks.exists(b1 => b.minSep.intersect(b1.minSep) == b && b != b1)
    def sepInclusionMinimal(s: Set[T]) = !g.allMinSep.exists(s1 => s.intersect(s1) == s && s != s1)
    def computeMfi(b: Block[T]) = if (blockInclusionMinimal(b)) Some(fill(b.vertices)) else None
    def fill(vs: Set[T]) = g.subGraph(vs).fill

    for (b <- blocks) {
      mfi = mfi.updated(b, computeMfi(b))
      for (c <- cliques if (b.minSep subsetOf c) && (b.minSep != c) && (c subsetOf b.vertices)) {// S subset c subsetEq (S,C)  [b = (S,C)]
        val bs = g.fullBlocks(c).filter(b1 => (b1.vertices subsetOf b.vertices) && b1 != b)
        println(bs)
        mfi = mfi.updated(b,
          Some(math.min(mfi(b),
                   fill(c) - fill(b.minSep) + bs.map(mfi(_)).foldLeft[Option[Int]](Some(0))(optionAdd(_, _)))))
      }
    }

    g.allMinSep.filter(sepInclusionMinimal(_)).minBy(s =>
      fill(s) + blocks.filter(_.minSep == s).map(mfi(_)).foldLeft[Option[Int]](Some(0))(optionAdd(_, _)))
  }
}