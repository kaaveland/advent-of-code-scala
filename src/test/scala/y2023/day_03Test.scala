package com.kaaveland.aoc
package y2023

import y2023.day_03.SchematicNumber

import org.scalatest.funsuite.AnyFunSuiteLike

class day_03Test extends AnyFunSuiteLike {
  test("find numbers") {
    val ex = "..123..\n..45#6"
    val found = y2023.day_03.findNumbers(ex)
    assert(found.size == 3)
    assert(
      found.forall(found =>
        List(SchematicNumber(2, 0, 123), SchematicNumber(2, 1, 45), SchematicNumber(5, 1, 6)).contains(found)
      )
    )
  }

  test("schematic number neighbours") {
    val ex = SchematicNumber(2, 0, 123)
    assert(ex.digits == 3)
    val shouldHave = List(
      (1, -1),
      (1, 0),
      (1, 1),
      (2, -1),
      (2, 1),
      (3, -1),
      (3, 1),
      (4, -1),
      (4, 1),
      (5, -1),
      (5, 0),
      (5, 1)
    )
    assert(shouldHave.forall(ex.neighbours.contains))
  }
}
