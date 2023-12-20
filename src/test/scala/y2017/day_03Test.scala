package com.kaaveland.aoc
package y2017

import org.scalatest.funsuite.AnyFunSuiteLike

class day_03Test extends AnyFunSuiteLike {
  test("Should run some code") {
    assert(day_03.findLayer(1) == 1)
    assert(day_03.findLayer(2) == 3)
    assert(day_03.findLayer(9) == 3)
    assert(day_03.findLayer(12) == 5)
    assert(day_03.findLayer(25) == 5)
    assert(day_03.findLayer(27) == 7)
    assert(day_03.findLayer(36) == 7)
    assert(day_03.findLayer(49) == 7)
    assert(day_03.findLayer(50) == 9)
  }
  test("Should also run some code") {
    assert(day_03.spiralDistance(11) == 2)
    assert(day_03.spiralDistance(23) == 2)
    assert(day_03.spiralDistance(17) == 4)
    assert(day_03.spiralDistance(24) == 3)
    assert(day_03.spiralDistance(26) == 5)
  }
}
