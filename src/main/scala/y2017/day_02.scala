package com.kaaveland.aoc
package y2017

import cats.effect.IO

object day_02 {
  def checksum(s: String): Int =
    s.split('\n').map(_.split("[\t ]+").map(_.toInt)).map(row => row.max - row.min).toList.sum
  def product[T](it: Iterable[T]): Iterable[(T, T)] = for {
    lhs <- it
    rhs <- it
  } yield (lhs, rhs)

  def divisibleChecksum(s: String): Int =
    s.split('\n')
      .map(_.split("[\t ]+").map(_.toInt))
      .map(
        product(_)
          .find {
            case (lhs, rhs) if rhs != 0 && lhs != rhs => lhs % rhs == 0
            case _ => false
          }
          .map { case (lhs, rhs) => lhs / rhs }
          .get
      )
      .sum
  def part1(s: String): IO[String] = IO {
    checksum(s.trim).toString
  }
  def part2(s: String): IO[String] = IO {
    divisibleChecksum(s.trim).toString
  }
}
