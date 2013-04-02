package com.kata

/**
 * Helper Class to execute and find journey possible.
 * User: PM
 * Date: 01/04/13
 */
object Execute {

  def main(args: Array[String]) {
    // Get the graph representation of the City Map from String representation of Map
    val graph = Routes.constructMap("a=b,c=d,a-c,b-d,d=e,b=c,e-f,f-g,f-h.L&M,LM")
    // Check if route possible
    println(Routes.isJourneyPossible(graph, "e", "f", false))
    println(Routes.isJourneyPossible(graph, "e", "f", true))
    println(Routes.isJourneyPossible(graph, "a", "d", false))
  }

}
