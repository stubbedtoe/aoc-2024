# Advent of Code Solutions / (Attempts) 2024

sbt project compiled with Scala 3

## First time set up

1. Install the required tools, Scala, SBT
2. Run `cp .env.example .env` at the root of the folder and set your Github token from the AoC website
3. Make the `addDay.sh` script executable by running `chmod +x addDay.sh`

## Adding a new day

At the root of the project, run `addDay.sh {dayIndex}` where `dayIndex` is the date in December you want to start, e.g. `3`.

## Useful commands in SBT

- `~run [dayIndex] [One|Two] [Test|Actual]` - this runs the specified day's code, either part One or Two, using either the Test or Actual input files (in watch mode)
- `test` - run all tests
- `~testOnly Day<dayIndex>` - run one day's tests in watch mode

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).
