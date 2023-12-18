package com.kaaveland.aoc
package y2017

import y2017.day_02.{checksum, divisibleChecksum}

import org.scalatest.funsuite.AnyFunSuiteLike

class day_02Test extends AnyFunSuiteLike {
  test("checksum examples") {
    val ex = "5 1 9 5\n7 5 3\n2 4 6 8";
    assert(checksum(ex) == 18)
  }
  test("checksum divisible examples") {
    val ex = "5 9 2 8\n9 4 7 3\n3 8 6 5"
    assert(divisibleChecksum(ex) == 9)
  }
}
