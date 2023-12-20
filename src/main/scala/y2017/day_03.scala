package com.kaaveland.aoc
package y2017

import cats.effect.IO

object day_03 {
  def findLayer(square: Int): Int = {
    Iterator
      .iterate(1)(_ + 2)
      .dropWhile(n => n * n < square)
      .nextOption
      .getOrElse(0)
  }
  def spiralDistance(n: Int): Int = {
    val layer = findLayer(n)
    val corners = (0 to 4).map(k => layer * layer - k * (layer - 1))
    corners
      .map(c => (c - n).abs)
      .find { dist => dist <= (layer - 1) / 2 }
      .map { dist => layer - 1 - dist }
      .get
  }
  def part1(n: String): IO[String] = IO {
    spiralDistance(n.trim.toInt).toString
  }

}
