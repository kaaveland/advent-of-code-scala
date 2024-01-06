package com.kaaveland.aoc
package y2023

import org.scalatest.funsuite.AnyFunSuiteLike

class day_01Test extends AnyFunSuiteLike {
  test("should translate digit in str correctly") {
    val ex = "123".toCharArray
    assert(day_01.numericValue(ex).contains(1))
    assert(day_01.numericValue(ex.tail).contains(2))
    assert(day_01.numericValue(ex.tail.tail.tail).isEmpty)
  }
  test("part1") {
    val ex = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
    assert(day_01.part1(ex) == 142)
  }
  test("part2") {
    val ex = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
    assert(day_01.part2(ex) == 281)
  }
}
