package com.kata

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import junit.framework.Assert

/**
 * User: Pradeep Muralidharan
 * Date: 01/04/13
 */
class CityMapKataTest extends FunSuite with ShouldMatchers {

  test("Graph should be empty if empty string is passed") {
    val graph = Routes.constructMap("")
    assert(graph.isEmpty == true)
  }

  test("Graph should not be empty when valid string representation of Map is passed"){
    val graph = Routes.constructMap("a=b")
    assert(graph.isEmpty == false)
  }

  test("Graph should not contain invalid or empty nodes"){
    val graph = Routes.constructMap("a=b,c")
    val x = graph.get("a")
    Assert.assertNotNull(x)
    intercept[NoSuchElementException]{
      graph.get("c")
    }
  }

  test("Graph must contain all relevant nodes for a valid string "){
    val graph = Routes.constructMap("a=b,b-c,b=d")
    val a = graph.get("a")
    val b = graph.get("b")
    val c = graph.get("c")
    val d = graph.get("d")

    Assert.assertNotNull(a)
    Assert.assertNotNull(b)
    Assert.assertNotNull(c)
    Assert.assertNotNull(d)
  }

  test("Happy Path :Is journey possible by Foot"){
    val graph = Routes.constructMap("a-b,b-c,c-d")
    assert(Routes.isJourneyPossible(graph,"a","d",true))
  }

  test("Failure scenario :Is journey possible by Foot"){
    val graph = Routes.constructMap("a-b,b=c,b-d,e-f")
    assert(Routes.isJourneyPossible(graph,"a","f",true) == false)
  }

  test("Happy Path :Is journey possible by Car"){
    val graph = Routes.constructMap("a=b,b=c,c=d,d=e,e=f")
    assert(Routes.isJourneyPossible(graph,"a","f",false))
  }

  test("Failure scenario :Is journey possible by Car"){
    val graph = Routes.constructMap("a-b,b=c,b-d,d-f")
    assert(Routes.isJourneyPossible(graph,"a","f",false) == false)
  }
}
