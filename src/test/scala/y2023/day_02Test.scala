package com.kaaveland.aoc
package y2023

import org.scalatest.funsuite.AnyFunSuiteLike

class day_02Test extends AnyFunSuiteLike {
  test("can parse a game") {
    import y2023.day_02.Attempt
    val text = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    assert(day_02.gameFromLine(text) == day_02.Game(1, List(Attempt(4, 3, 0), Attempt(1, 6, 2), Attempt(0, 0, 2))))
  }
}
