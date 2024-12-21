package com.kaaveland.aoc
package y2024

import cats.effect.IO

import java.util
import scala.collection.mutable

object day_20 {
  type Pos = (Int, Int)

  def parseMaze(inp: String): Array[(Int, Int, Char)] =
    inp.split('\n').zipWithIndex.flatMap { (line, y) =>
      line.zipWithIndex.map { (ch, x) => (x, y, ch) }
    }

  case class Maze(passable: Set[Pos], start: Pos, end: Pos)
  object Maze {
    def apply(inp: String): Option[Maze] = {
      val m = parseMaze(inp)
      def coords(t: (Int, Int, Char)) = (t._1, t._2)
      val start = m.find(_._3 == 'S').map(coords)
      val end = m.find(_._3 == 'E').map(coords)
      val passable = m.filter(_._3 != '#').map(coords)

      for {
        s <- start
        e <- end
      } yield new Maze(passable.toSet, s, e)
    }
  }

  def bfs(tiles: Set[Pos], pos: Pos): (java.util.HashMap[Int, Int], Map[Pos, Pos]) = {
    val work = mutable.Queue.empty[(Pos, Option[Pos], Int)]
    work.enqueue((pos, None, 0))
    val distances = new util.HashMap[Int, Int]()
    val parents = mutable.HashMap.empty[Pos, Pos]

    while (work.nonEmpty) {
      val (pos, parent, cost) = work.dequeue()
      if (tiles.contains(pos) && !distances.containsKey(key(pos))) {
        distances.put(key(pos), cost)
        parent.foreach(p => { parents(pos) = p; })
        List((0, 1), (0, -1), (1, 0), (-1, 0)).foreach { (dx, dy) =>
          work.enqueue(((pos._1 + dx, pos._2 + dy), Some(pos), cost + 1))
        }
      }
    }

    (distances, parents.toMap)
  }

  def path(parents: Map[Pos, Pos], pos: Pos): List[Pos] = {
    var path: List[Pos] = List()
    var cur: Option[Pos] = Some(pos)
    while (cur.isDefined) {
      val p = cur.get
      path = p :: path
      cur = parents.get(p)
    }
    path
  }

  def manhattanOffsets(cheatLen: Int): IndexedSeq[(Int, Int, Int)] = {
    (for {
      dx <- -cheatLen `to` cheatLen
      maxDy = cheatLen - dx.abs
      dy <- -maxDy `to` maxDy
    } yield (dx, dy, dx.abs + dy.abs))
  }

  def key(pos: (Int, Int)): Int = {
    val (x, y) = pos
    (x & 0xff) | ((y & 0xff) << 8)
  }

  def findCheats(distances: java.util.HashMap[Int, Int], path: List[Pos], cheatLen: Int, minGain: Int): Int = {
    val offsets = manhattanOffsets(cheatLen)
    var count = 0
    for {
      (x, y) <- path
      remaining = distances.get((x & 0xff) | ((y & 0xff) << 8))
      (dx, dy, cost) <- offsets
      (nx, ny) = (x + dx, y + dy)
      gain = distances.getOrDefault((nx & 0xff) | ((ny & 0xff) << 8), -100000) - cost - remaining
      if gain >= minGain
    } {
      count += 1
    }
    count
  }

  def solve(inp: String, cheatLen: Int, minGain: Int): Option[Int] = for {
    maze <- Maze(inp)
    (distances, parents) = bfs(maze.passable, maze.end)
  } yield findCheats(distances, path(parents, maze.start), cheatLen, minGain)

  def part1(inp: String): IO[String] = IO
    .apply {
      solve(inp, 2, 100)
    }
    .flatMap {
      case Some(solution) => IO.pure(solution.toString)
      case None => IO.raiseError(new RuntimeException("Unable to solve maze"))
    }

  def part2(inp: String): IO[String] = IO
    .apply {
      solve(inp, 20, 100)
    }
    .flatMap {
      case Some(solution) => IO.pure(solution.toString)
      case None => IO.raiseError(new RuntimeException("Unable to solve maze"))
    }
}
