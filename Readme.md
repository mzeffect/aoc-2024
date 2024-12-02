# AoC 2024

Solutions for [Advent of Code 2024](https://adventofcode.com/2024).

### Prerequisites

- .NET SDK 9 ([instructions](https://dotnet.microsoft.com/en-us/download))

### How to run

Install dependencies:

```shell
dotnet restore
```

Run the first part of the puzzle for a specific day, use the `a` suffix:

```shell
dotnet run Day01a
```

To run the second part, use `b`:

```shell
dotnet run Day01b
```

### Add a new solver

As an example, here's how to add a new solver for day 13, first part:

1. Add a new F# file, e.g. `Day13a.fs`
2. Create a new folder in `/data`, e.g. `/data/Day13`
3. Your puzzle input should be in `/data/Day13/input.txt`
4. Put the puzzle's example input into `/data/Day13/examples/13a.txt`
5. Put the expected output for the example into `/data/Day13/examples/13a_out.txt`

The examples act as tests, they will be passed to the solver and the result will be compared to the expected output.

You will be notified if your code fails to pass on the examples.

### Write the solver

A solver is a function that takes a string (input) and returns a string (solution).

It has to be called `solve` and must be in a module with the same name as the day + puzzle part, e.g. `Day13a`. This is because the runner uses reflection to find the solver for each puzzle.

```fsharp
module AdventOfCode2024.Day13a // important to name the module like this, so it can be executed with `dotnet run Day13a`

open AdventOfCode2024.Utils

let solve (input: string) =
    "implement me"
```

Of course, you can structure your code however you want, but the runner will only look for and execute the `solve` function.
