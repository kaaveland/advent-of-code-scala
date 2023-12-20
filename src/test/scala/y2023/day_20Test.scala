package com.kaaveland.aoc
package y2023

import org.scalatest.funsuite.AnyFunSuiteLike

class day_20Test extends AnyFunSuiteLike {
  import day_20._
  test("should work fine for example") {
    val example = "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a"
    val Right((_, machines)) = day_20.parser.parse(example): @unchecked
    assert(machines.head == Machine(MachineKind.Broadcast, "broadcaster", List("a", "b", "c")))
    val byNames = day_20.machinesByName(machines.toList)
    assert(byNames.size == 5)
    val (lo, hi, _) = day_20.simulate(byNames, "rx", false)
    assert(lo == 8)
    assert(hi == 4)
  }

  test("should work fine for second example") {
    val example = "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output"
    val Right((_, machines)) = day_20.parser.parse(example): @unchecked
    val byNames = day_20.machinesByName(machines.toList)
    {
      val (lo, hi, _) = day_20.simulate(byNames, "rx", false)
      assert(lo == 4)
      assert(hi == 4)
    }
    {
      val (lo, hi, _) = day_20.simulate(byNames, "rx", false)
      assert(lo == 4)
      assert(hi == 2)
    }
    {
      val (lo, hi, _) = day_20.simulate(byNames, "rx", false)
      assert(lo == 5)
      assert(hi == 3)
    }
  }

}
