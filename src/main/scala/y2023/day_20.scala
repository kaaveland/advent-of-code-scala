package com.kaaveland.aoc
package y2023

import cats.data.NonEmptyList
import cats.effect.IO
import cats.parse.{Parser, Parser0, Rfc5234}

import scala.collection.{immutable, mutable}
object day_20 {
  import cats.implicits._

  sealed trait MachineKind
  object MachineKind {
    case object Broadcast extends MachineKind
    case object FlipFlop extends MachineKind
    case object Conj extends MachineKind
  }

  case class Machine(kind: MachineKind, name: String, outputs: List[String])

  sealed trait Memory
  private object Memory {
    case class FlipFlop(var on: Boolean) extends Memory
    case class Conj(mem: mutable.Map[String, Boolean]) extends Memory
    case object Broadcast extends Memory
  }
  private val machineKind: Parser0[MachineKind] =
    Parser.char('&').as(MachineKind.Conj) | Parser.char('%').as(MachineKind.FlipFlop) | Parser.pure(
      MachineKind.Broadcast
    )
  private val name = Rfc5234.alpha.rep.string
  private val outputs = name.repSep0(Parser.string(", "))
  val parser: Parser[NonEmptyList[Machine]] =
    ((machineKind.with1 ~ name) ~ (Parser.string(" -> ").void *> outputs <* Parser.char('\n').?)).map {
      case ((kind, name), outputs) =>
        Machine(kind, name, outputs)
    }.rep

  case class SimulatedMachine(name: String, memory: Memory, outputs: List[String]) {
    def receiveSignal(signal: Boolean, source: String): Option[Boolean] = memory match {
      case Memory.Broadcast => Some(signal)
      case mem @ Memory.FlipFlop(false) if !signal =>
        mem.on = true
        Some(true)
      case mem @ Memory.FlipFlop(true) if !signal =>
        mem.on = false
        Some(false)
      case Memory.Conj(sources) =>
        sources.put(source, signal)
        Some(!sources.values.forall(on => on))
      case _ => None
    }
  }

  def part1(inp: String): IO[String] = {
    toMachineMap(inp) match {
      case Right(map) =>
        IO {
          val r = pressButton(map)
          (r._1 * r._2).toString
        }
      case Left(err) => IO.raiseError(new RuntimeException(err.toString))
    }
  }

  private def findPeriod(inp: String, node: String, lookFor: Boolean): Either[Parser.Error, Long] =
    toMachineMap(inp).map { map =>
      Iterator
        .iterate(1)(_ + 1)
        .find(n => simulate(map, node, lookFor)._3 > 0)
        .get
    }

  def part2(inp: String): IO[String] = toMachineMap(inp) match {
    case Right(map) =>
      IO {
        val pointsToRx = map.find { (_, v) => v.outputs.contains("rx") }.get._1
        map.iterator.filter { (_, v) => v.outputs.contains(pointsToRx) }.map(_._1).toList
      }.flatMap(pointsThere => pointsThere.parTraverse(node => IO(findPeriod(inp, node, false).getOrElse(1L)))).map {
        r => r.product.toString
      }
    case Left(err) => IO.raiseError(new RuntimeException(err.toString))
  }

  private def toMachineMap(s: String): Either[Parser.Error, Map[String, SimulatedMachine]] =
    parser.parse(s).map { case (_, machines) => machinesByName(machines.toList) }

  def machinesByName(machines: List[Machine]): immutable.Map[String, SimulatedMachine] = {
    val destToSource = machines
      .flatMap { source =>
        source.outputs.map { dest => (dest, source.name) }
      }
      .groupBy(_._1)
      .view
      .mapValues(v => v.map(_._2))

    machines.map { machine =>
      val memory = machine.kind match {
        case MachineKind.Broadcast => Memory.Broadcast
        case MachineKind.FlipFlop => Memory.FlipFlop(false)
        case MachineKind.Conj =>
          Memory.Conj(
            mutable.Map.from(destToSource(machine.name).map(source => source -> false).toMap)
          )
      }
      machine.name -> SimulatedMachine(machine.name, memory, machine.outputs)
    }.toMap
  }

  private def pressButton(machineMap: Map[String, SimulatedMachine]): (Long, Long, Long) = {
    (0 to 999)
      .map(_ => simulate(machineMap, "rx", false))
      .fold((0L, 0L, 0L))((left, right) => (left._1 + right._1, left._2 + right._2, left._3 + right._3))
  }

  def simulate(
      machineMap: Map[String, SimulatedMachine],
      observe: String,
      lookFor: Boolean
  ): (Long, Long, Long) = {
    val work = new mutable.ArrayDeque[(String, String, Boolean)]()
    work.append(("button", "broadcaster", false))
    var (lo, hi, found) = (0L, 0L, 0L)
    while (work.nonEmpty) {
      work.removeHead() match {
        case (source, dest, signal) =>
          if (signal) {
            hi += 1
          } else {
            lo += 1
          }
          if (dest == observe && lookFor == signal) {
            found += 1
          }
          machineMap.get(dest) match {
            case Some(recipient) =>
              recipient.receiveSignal(signal, source) match {
                case Some(nextSignal) =>
                  work.addAll(recipient.outputs.map { next => (dest, next, nextSignal) })
                case _ => ()
              }
            case _ => ()
          }
      }
    }
    (lo, hi, found)
  }

}
