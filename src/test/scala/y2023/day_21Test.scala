package com.kaaveland.aoc
package y2023

import org.scalatest.funsuite.AnyFunSuiteLike

class day_21Test extends AnyFunSuiteLike {
  val ex = "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n..........."
  test("can parse ex") {
    val g = Grid(ex)
    assert(g.width == 11)
    val s = g.findStart
    assert(s.contains((5, 5)))
    val walk = g.walkFrom((5, 5), 6)
    assert(walk(0) == 1)
    assert(walk(1) == 2)
    assert(walk(6) == 16)

  }
}
