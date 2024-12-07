package com.kaaveland.aoc
package y2024

import cats.effect.IO

object day_01 {
  private def parse_ints(inp: String): (List[Int], List[Int]) = {
    val (even, odd) =
      inp.split(Array('\n', ' ', '\t')).filterNot(_.isEmpty).map(_.toInt).zipWithIndex.partition { case (_, index) =>
        index % 2 == 0
      }
    (even.map(_._1).toList, odd.map(_._1).toList)
  }
  def part1(input: String): IO[String] = IO {
    val (left, right) = parse_ints(input)
    left.sorted.zip(right.sorted).map { case (l, r) => (l - r).abs }.sum.toString
  }
  def part2(input: String): IO[String] = IO {
    val (left, right) = parse_ints(input)
    val counter = right.groupBy(identity).view.mapValues(_.size).toMap
    left.map { n => n * counter.getOrElse(n, 0) }.sum.toString
  }
}
