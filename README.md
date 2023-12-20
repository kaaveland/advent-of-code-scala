advent-of-code-scala
==

These are some advent of code solutions in scala. It's easiest
to run them from the `sbt` shell:

```shell
sbt:advent-of-code-4s> run
[info] running (fork) com.kaaveland.aoc.main
[info] aoc <command> [arguments] -- Advent of Code solutions
[info] possible commands:
[info]  help: show this message
[info]  run: run a solution, arguments: [year default latest] [day default latest]
[info]  data: fetch data for the problems, arguments: [year default latest] [day default latest]
```

To run the solutions for a year:
```shell
sbt:advent-of-code-4s> run run 2023
[info] running (fork) com.kaaveland.aoc.main run 2023
[info] Year 2023 day 20 part 1: 4ms
[info] 925955316
[info] Year 2023 day 20 part 2: 28ms
[info] 241528477694627
[info] Total time including I/O: 32ms
```

There's setup to run it as a native image, with `sbt nativeImage`:

```shell
target/native-image/advent-of-code-4s run 2023
Year 2023 day 20 part 1: 4ms
925955316
Year 2023 day 20 part 2: 44ms
241528477694627
Total time including I/O: 50ms
```
