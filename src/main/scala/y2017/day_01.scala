package com.kaaveland.aoc
package y2017

import cats.effect.IO

object day_01 {
  def sumMatchingDigits(inp: String, skip: Int): Int = inp
    .zip(inp.drop(skip) + inp.take(skip))
    .map {
      case (before, after) if before == after => before.toInt - '0'.toInt
      case _ => 0
    }
    .sum

  def part1(inp: String): IO[String] = IO {
    sumMatchingDigits(inp.trim, 1).toString
  }
  def part2(inp: String): IO[String] = IO {
    val n = inp.trim
    sumMatchingDigits(n, n.length / 2).toString
  }
}
