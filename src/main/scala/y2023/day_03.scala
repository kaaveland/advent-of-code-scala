package com.kaaveland.aoc
package y2023

object day_03 {

  trait Grid {
    def get(x: Int, y: Int): Option[Char]
    def width: Int
    def height: Int
    def isSym(x: Int, y: Int): Boolean = get(x, y) match {
      case Some(c) => c != '.' && !c.isDigit
      case _ => false
    }
  }

  def toGrid(input: String): Grid = new Grid {
    private val chars = input.toCharArray.filter(_ != '\n')
    private val w = input.indexOf('\n')
    override def width: Int = w
    override def height: Int = chars.length / width
    override def get(x: Int, y: Int): Option[Char] =
      if (x < 0 || y < 0 || x >= width || y >= height) None else Some(chars(y * width + x))
  }
  case class SchematicNumber(x: Int, y: Int, number: Int) {
    def digits: Int = number.toString.length
    val neighbours: Set[(Int, Int)] = ((x - 1) to (x + digits) flatMap { x =>
      (y - 1) to (y + 1) map { y =>
        (x, y)
      }
    }).toSet
  }
  def findNumbers(input: String): List[SchematicNumber] = input.linesIterator.zipWithIndex.flatMap { (line, y) =>
    val (started, finished) = line.zipWithIndex
      .foldLeft((Option.empty[Int], List.empty[SchematicNumber])) {
        case ((None, acc), (ch, x)) if ch.isDigit => (Some(x), acc)
        case ((Some(start), acc), (ch, x)) if !ch.isDigit =>
          val n = line.slice(start, x).toInt
          (None, SchematicNumber(start, y, n) :: acc)
        case (acc, _) => acc
      }
    started
      .map { start => SchematicNumber(start, y, line.slice(start, line.length).toInt) :: finished }
      .getOrElse(finished)
  }.toList

  def part1(input: String): Int = {
    val grid = toGrid(input)
    val numbers = findNumbers(input)
    numbers
      .filter { n =>
        n.neighbours.exists { case (x, y) => grid.isSym(x, y) }
      }
      .map(_.number)
      .sum
  }

  def part2(input: String): Int = {
    val numbers = findNumbers(input)
    input.linesIterator.zipWithIndex
      .flatMap { (line, y) =>
        line.zipWithIndex
          .filter { case (ch, _) => ch == '*' }
          .map { case (_, x) => (x, y) }
      }
      .map { case (x, y) =>
        numbers.filter(n => n.neighbours.contains((x, y))) match {
          case left :: right :: Nil => left.number * right.number
          case _ => 0
        }
      }
      .sum
  }
}
