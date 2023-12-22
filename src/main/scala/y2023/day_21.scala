package com.kaaveland.aoc
package y2023

import cats.effect.IO

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

def remEuclid(dividend: Int, divisor: Int): Int = {
  val remainder = dividend % divisor
  if (remainder < 0) remainder + divisor else remainder
}

case class Grid(chars: Array[Char], width: Int) {
  def at(x: Int, y: Int): Option[Char] = {
    Some(chars(remEuclid(x, width) + width * remEuclid(y, width)))
  }
  def findStart: Option[(Int, Int)] = chars.zipWithIndex
    .find { case (ch, _) =>
      ch == 'S'
    }
    .map { case (_, i) =>
      (i % width, i / width)
    }
  def walkFrom(start: (Int, Int), time: Int): Array[Long] = {
    val directions: List[(Int, Int)] = List((0, 1), (0, -1), (1, 0), (-1, 0))
    val distances = mutable.Map[(Int, Int), Int]()
    val work = mutable.Queue[(Int, (Int, Int))]()
    work.enqueue((0, start))
    distances.put(start, 0)
    boundary {
      while (work.nonEmpty) {
        val (t, (x, y)) = work.dequeue()
        if (t >= time) {
          break()
        }
        directions
          .map { case (dx, dy) => (x + dx, y + dy) }
          .filter { case (x, y) => at(x, y).exists(ch => "S.".contains(ch)) }
          .filterNot(distances.contains)
          .foreach { next =>
            distances.put(next, t + 1)
            work.enqueue((t + 1, next))
          }

      }
    }
    val counts = Array.fill(time + 1)(0L)
    distances.values.foreach(v => {
      counts(v) = counts(v) + 1
    })
    var (evens, odds) = (0L, 0L)
    counts.zipWithIndex.map { case (c, t) =>
      if (t % 2 == 0) {
        evens += c
        evens
      } else {
        odds += c
        odds
      }
    }
  }
}

object Grid {

  def apply(s: String): Grid = {
    val chars = s.filter(ch => "#.S".contains(ch)).toCharArray
    val width = chars.length / s.linesIterator.next().length
    new Grid(chars, width)
  }

  def walk(s: String, steps: Int): Array[Long] = {
    val grid = Grid(s)
    val start = grid.findStart.get
    grid.walkFrom(start, steps)
  }
}
object day_21 {
  def part1(s: String): IO[String] = IO {
    Grid.walk(s, 64).last.toString
  }

  def part2(s: String): IO[String] = IO {
    val grid = Grid(s)
    val start = grid.findStart.get
    val stepCount = 26501365
    val gridRadius = stepCount / grid.width
    val remainder = remEuclid(stepCount, grid.width)
    val sample = grid.walkFrom(start, grid.width * 2 + remainder + 1)
    val f0 = sample(remainder)
    val f1 = sample(remainder + grid.width)
    val f2 = sample(remainder + grid.width * 2)
    val b = (4 * f1 - 3 * f0 - f2) / 2
    val a = f1 - b - f0
    (a * gridRadius * gridRadius + b * gridRadius + f0).toString
  }
}
