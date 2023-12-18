package com.kaaveland.aoc
package y2017

import org.scalatest.funsuite.AnyFunSuite
import day_01._

class day_01Test extends AnyFunSuite {
  test("day 1 example") {
    assert(sumMatchingDigits("91212129", 1) == 9)
    assert(sumMatchingDigits("1234", 1) == 0)
    assert(sumMatchingDigits("1111", 1) == 4)
    assert(sumMatchingDigits("1212", 2) == 6)
    assert(sumMatchingDigits("123123", 3) == 12)
    assert(sumMatchingDigits("12131415", 4) == 4)
  }
}
