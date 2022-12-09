# Advent of code 2022

## Kézako?
More info [here](https://adventofcode.com/2022/about).

## Structure
Most (if not all) solutions use the same [skeleton](src/Template.sc).
To summarize it: 
- An input parsing function;
- One function for each (of the two) questions;
- Some assertions testing the provided test case(s) (inputs are [here](tests));
- Some prints for the solution (inputs are [here](inputs))

## How to run
Written in Scala 3, built with [scala-cli](https://scala-cli.virtuslab.org/).
```
scala-cli run src/DayXX.sc
```