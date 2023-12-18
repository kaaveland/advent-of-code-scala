package com.kaaveland.aoc

import AdventOfCodeInputs.{dataFor, storeYear}

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

val solutions: Map[Int, Map[Int, Solution]] = Map(
  2017 -> Map(
    1 -> Solution(y2017.day_01.part1, y2017.day_01.part2),
    2 -> Solution(_ => IO("Not implemented yet"), _ => IO("Not implemented yet"))
  )
)

sealed trait TimeCoordinates
case class Year(year: Int) extends TimeCoordinates
case class YearDay(year: Int, day: Int) extends TimeCoordinates

object Year {
  def valid(year: Int): Either[String, Year] = if (year >= 2015 && year <= 2023) {
    Right(Year(year))
  } else { Left(s"No such year: $year") }
}

object YearDay {
  def valid(year: Int, day: Int): Either[String, YearDay] = if (day >= 1 && day <= 25) {
    Year.valid(year).map(_ => YearDay(year, day))
  } else { Left(s"No such day: $day") }
}

def parseTime(args: List[String]): Either[String, TimeCoordinates] = args match {
  case year :: day :: Nil if year.matches("[0-9]+") && day.matches("[0-9]+") =>
    Right(YearDay(year.toInt, day.toInt))
  case year :: Nil if year.matches("[0-9]+") => Year.valid(year.toInt)
  case _ => Left("Not a valid year/day combination")
}

case class Solution(part1: String => IO[String], part2: String => IO[String])

def lookup(coordinates: TimeCoordinates): List[(Int, Int, Solution)] = coordinates match {
  case Year(y) => solutions.getOrElse(y, Map.empty).map((day, sol) => (y, day, sol)).toList
  case YearDay(y, d) =>
    solutions.getOrElse(y, Map.empty).get(d).map(sol => List((y, d, sol))).getOrElse(List.empty)
}

def runSolutions(coordinates: TimeCoordinates): IO[String] = {
  val results = lookup(coordinates).parTraverse { case (year, day, sol) =>
    for {
      inp <- dataFor(year, day)
      before <- IO(System.currentTimeMillis())
      p1 <- sol.part1(inp)
      afterP1 <- IO(System.currentTimeMillis())
      p2 <- sol.part2(inp)
      afterP2 <- IO(System.currentTimeMillis())
      rt1 = String.format("%d", afterP1 - before)
      rt2 = String.format("%d", afterP2 - afterP1)
    } yield (
      year,
      day,
      s"Year $year day $day part 1: ${rt1}ms\n$p1\nYear $year day $day part 2: ${rt2}ms\n$p2\n"
    )
  }
  val all = results.map { r =>
    r.sorted
      .map { case (_, _, s) =>
        s
      }
      .mkString("\n")
  }
  for {
    before <- IO(System.currentTimeMillis())
    text <- all
    after <- IO(System.currentTimeMillis())
  } yield s"$text\nTotal time including I/O: ${after - before}ms\n"
}

object main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = args.map(_.toLowerCase()) match {
    case "run" :: tail => runSolution(tail)
    case "data" :: tail => data(tail)
    case "help" :: _ | _ => help
  }

  def help: IO[ExitCode] = IO.println("""
      |aoc <command> [arguments] -- Advent of Code solutions
      |possible commands:
      | help: show this message
      | run: run a solution, arguments: [year default latest] [day default latest]
      | data: fetch data for the problems, arguments: [year default latest] [day default latest]
      |""".stripMargin) >> IO(ExitCode.Success)

  def runSolution(args: List[String]): IO[ExitCode] = parseTime(args) match {
    case Right(tc) =>
      runSolutions(tc).flatMap(results => IO.print(results)).map(_ => ExitCode.Success)
    case Left(err) => IO.println(err).map(_ => ExitCode.Error)
  }

  def data(args: List[String]): IO[ExitCode] = parseTime(args) match {
    case Right(Year(y)) =>
      storeYear(y).flatMap(_ => IO.println(s"Saved data for $y")).map(_ => ExitCode.Success)
    case Right(YearDay(y, d)) => dataFor(y, d).flatMap(IO.println).map(_ => ExitCode.Success)
    case Left(err) => IO.println(err).map(_ => ExitCode.Error)
  }
}
