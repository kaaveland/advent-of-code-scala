package com.kaaveland.aoc
package y2024

import cats.effect.IO
import cats.implicits.*

import scala.collection.mutable

def coordsOf(inputGrid: String, ch: Char): Array[(Int, Int)] = {
  val lines = inputGrid.split("\n").filterNot(_.isEmpty)
  for {
    (line, y) <- lines.zipWithIndex
    (char, x) <- line.zipWithIndex
    if char == ch
  } yield (x, y)
}

trait Grid {
  val height: Int
  val width: Int
  def obstacleAt(x: Int, y: Int): Boolean
  def contains(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
}

class StringGrid(inputGrid: String) extends Grid {
  val height = inputGrid.split("\n").length
  val width = inputGrid.split("\n").head.length
  val grid = inputGrid.replace("\n", "").toCharArray

  override def obstacleAt(x: Int, y: Int): Boolean = grid(y * width + x) == '#'
}

class SetGrid(inputGrid: String) extends Grid {
  val lines = inputGrid.split("\n").filterNot(_.isEmpty)
  val height = lines.length
  val width = lines.head.length
  val obstacles = coordsOf(inputGrid, '#').toSet

  override def obstacleAt(x: Int, y: Int): Boolean = obstacles.contains((x, y))
}

val DIRECTIONS = List((0, -1), (1, 0), (0, 1), (-1, 0))
type CacheKey = ((Int, Int), Int)
type PathCache = Map[CacheKey, CacheKey]

def buildPathCache(grid: Grid, origin: (Int, Int)): PathCache = {
  var dir = 0
  val cache = mutable.Map.empty[CacheKey, CacheKey]
  var segmentKeys = List.empty[((Int, Int), Int)]
  var pos = origin

  def resolveSegment(pos: (Int, Int), dir: (Int)) = {
    for {
      (p, d) <- segmentKeys
    } {
      cache((p, d)) = (pos, dir)
    }
    segmentKeys = List.empty
  }

  while (true) {
    segmentKeys = (pos, dir) :: segmentKeys
    val (dx, dy) = DIRECTIONS(dir)
    val (x, y) = (pos._1 + dx, pos._2 + dy)
    if (grid.contains(x, y)) {
      if (grid.obstacleAt(x, y)) {
        dir = (dir + 1) % 4
        resolveSegment(pos, dir)
      } else {
        pos = (x, y)
      }
    } else {
      resolveSegment((x, y), dir)
      return cache.toMap
    }
  }
  cache.toMap
}

def loops(grid: Grid, origin: (Int, Int), cache: PathCache, obs: (Int, Int)): Boolean = {
  var dir = 0
  val visited = mutable.Set.empty[CacheKey]
  var pos = origin
  while (true) {
    if (visited.add((pos, dir))) {
      if (cache.contains((pos, dir)) && (pos._1 != obs._1 && pos._2 != obs._2)) {
        val (nextPos, nextDir) = cache((pos, dir))
        pos = nextPos
        dir = nextDir
      } else {
        val (dx, dy) = DIRECTIONS(dir)
        val (x, y) = (pos._1 + dx, pos._2 + dy)
        if (grid.contains(x, y)) {
          if (grid.obstacleAt(x, y) || (x, y) == obs) {
            dir = (dir + 1) % 4
          } else {
            pos = (x, y)
          }
        } else {
          return false
        }
      }
    } else {
      return true
    }
  }
  false
}

object day_06 {
  def part1(input: String): IO[String] = IO {
    val grid = new StringGrid(input)
    val origin = coordsOf(input, '^').head
    val pathCache = buildPathCache(grid, origin)
    val len = pathCache.keys.map(_._1).filter(grid.contains).toSet.size
    len.toString
  }
  def part2(input: String): IO[String] = IO {
    val grid = new StringGrid(input)
    val origin = coordsOf(input, '^').head
    val pathCache = buildPathCache(grid, origin)
    val path = pathCache.keys.map(_._1).filter(grid.contains).filter(pos => pos != origin).toSet
    path.toList.parTraverse { pos =>
      IO {
        loops(grid, origin, pathCache, pos)
      }
    }
  }.flatten.map(_.count(identity).toString)
}
