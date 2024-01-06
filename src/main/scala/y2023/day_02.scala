package com.kaaveland.aoc
package y2023

import scala.annotation.targetName

object day_02 {
  case class Attempt(red: Int, blue: Int, green: Int) {
    @targetName("add")
    def +(other: Attempt): Attempt = Attempt(red + other.red, blue + other.blue, green + other.green)
    def maxit(other: Attempt): Attempt = Attempt(red.max(other.red), blue.max(other.blue), green.max(other.green))
  }
  object Attempt {
    val zero: Attempt = Attempt(0, 0, 0)
  }
  case class Game(id: Int, attempts: List[Attempt])
  def gameFromLine(line: String): Game = {
    line.split(": ").toList match {
      case id :: attempts :: Nil =>
        Game(
          id.split(" ")(1).toInt,
          attempts
            .split("; ")
            .map { attempt =>
              attempt.split(", ").foldLeft(Attempt.zero) { (acc, color) =>
                val next = color.split(" ").toList match {
                  case value :: c :: Nil =>
                    c match {
                      case "red" => Attempt(value.toInt, 0, 0)
                      case "blue" => Attempt(0, value.toInt, 0)
                      case "green" => Attempt(0, 0, value.toInt)
                      case _ => throw new Exception(s"Invalid color: $c")
                    }
                  case _ => throw new Exception(s"Invalid color: $color")
                }
                next + acc
              }
            }
            .toList
        )
      case _ => throw new Exception(s"Invalid line: $line")
    }
  }

  def part1(s: String): Int = s.linesIterator
    .map(gameFromLine)
    .filter(g => g.attempts.forall(a => a.red <= 12 && a.green <= 13 && a.blue <= 14))
    .map(_.id)
    .sum

  def part2(s: String): Int = s.linesIterator
    .map(gameFromLine)
    .map(g => g.attempts.foldLeft(Attempt.zero)(_ maxit _))
    .map(a => a.red * a.blue * a.green)
    .sum
}
