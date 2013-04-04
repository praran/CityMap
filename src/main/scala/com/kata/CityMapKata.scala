package com.kata

import scalax.collection.Graph
import scala.None
import scalax.collection.edge.Implicits._
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge

/**
 * User: PM
 * Date: 01/04/13
 */

object Routes extends CityMapKata {

  /**
   * Check if journey possible
   * @param graph
   * @param frm
   * @param to
   * @param byFoot
   * @return
   */
  def isJourneyPossible(graph: scalax.collection.Graph[String, scalax.collection.edge.LDiEdge], frm: String, to: String, byFoot: Boolean): Boolean = {
    getJourneyPath(graph, frm, to, byFoot) match {
      case Some(x) => return true
      case None    => return false
    }
  }
}

sealed trait CityMapKata extends Extractors {
  /**
   * Construct the Graph representation of City Map using string representation of the Map
   * @param mapAsString
   * @return
   */
  def constructMap(mapAsString: String): scalax.collection.Graph[String, scalax.collection.edge.LDiEdge] = {
    // Construct and return cityMap Graph from nodes and edges
    return Graph.from(extractNodes(mapAsString), extractEdges(mapAsString))
  }

  /**
   * From the Graph representation of CityMap get the journey path
   * @param graph
   * @param frm
   * @param to
   * @param byFoot
   * @return
   */

  def getJourneyPath(graph: scalax.collection.Graph[String, scalax.collection.edge.LDiEdge], frm: String, to: String, byFoot: Boolean): Option[graph.Path] = {
    // extract the graph from the node
    def n(outer: String) = graph get outer
    return graph.isEmpty match {
          case false => byFoot match {
                          case true=>  n(frm) shortestPathTo n(to)
                          case false =>  n(frm) shortestPathTo(n(to), edgeFilter = _.label == MAP_TRAVEL_BOTH)
                        }
           case _   =>  None
       }
  }
}

sealed trait Extractors {
  // Constants
  val MAP_ROUTE_DELIMITER: String = ","
  val MAP_TRAVEL_BOTH:     String = "both"
  val MAP_TRAVEL_FOOT:     String = "foot"

  /**
   * Extract the edges between nodes from the string representation of the Map
   * @param str
   * @return
   */
  def extractEdges(str: String): Array[scalax.collection.edge.LDiEdge[String] with scalax.collection.GraphEdge.EdgeCopy[scalax.collection.edge.LDiEdge] {type L1 = String}] = {
    // Generate the Edge representation of the routes
    val vEdges = for {str <- str.split(MAP_ROUTE_DELIMITER)
                      if str.length == 3 && (str.charAt(1) == '=' || str.charAt(1) == '-')
                      frm = str.charAt(0).toString
                      to = str.charAt(2).toString
                      rType = if (str.charAt(1) == '=') MAP_TRAVEL_BOTH else MAP_TRAVEL_FOOT
    } yield (frm ~+> to)(rType)

    return vEdges
  }

  /**
   * Extract the distinct nodes from the string representation of the Map
   * @param str
   * @return
   */
  def extractNodes(str: String): List[String] = {
    // Extract all nodes from the string representation
    val vNL = for {str <- str.split(MAP_ROUTE_DELIMITER).toList
                   if str.length == 3 && (str.charAt(1) == '=' || str.charAt(1) == '-')
                   frm = str.charAt(0).toString
                   to = str.charAt(2).toString
                   rType = if (str.charAt(1) == '=') MAP_TRAVEL_BOTH else MAP_TRAVEL_FOOT
    } yield List(frm, to)
    // Extract and return Distinct Nodes
    return vNL.flatMap(x => x).distinct
  }
}