//> using lib "com.lihaoyi::os-lib:0.9.0"

import scala.collection.mutable.Stack

type State = Array[Stack[Char]]

def printState(s: State): Unit = {
  val max = s.map(_.length).max
  println(s.map(_.toArray.reverse.padTo(max, ' ').reverse).transpose.map(_.mkString(" ")).mkString("\n"))
}

def parseConfiguration(str: String): State = {
  val cols :: lines = str.split("\n").toList.reverse
  val colCount = cols.split("   ").length
  val res = Array.fill(colCount)(Stack.empty[Char])
  for {
    line <- lines;
    i <- 0 until colCount
  } do {
    val chr = line.charAt(1 + 4*i)
    if (chr != ' ') {
      res(i).push(chr)
    }
  }
  res
}

def parseMoves(str: String): List[(Int, Int, Int)] = {
  str.split("\n").map{ 
    case s"move ${i} from ${j} to ${k}" => (Integer.parseInt(i), Integer.parseInt(j)-1, Integer.parseInt(k)-1) 
  }.toList
}

def parseInput(path: String): (State, List[(Int, Int, Int)]) = {
  val in = os.read(os.pwd / path / "day_05.txt")
  val conf :: moves :: Nil = in.split("\n\n").toList
  (parseConfiguration(conf), parseMoves(moves))
}

type Mover = (State, (Int, Int, Int)) => Unit

def executeMove1(state: State, move: (Int, Int, Int)): Unit = {
  val (count, from, to) = move
  for (i <- 0 until count) do {
    state(to).push(state(from).pop())
  }
}

def executeMove2(state: State, move: (Int, Int, Int)): Unit = {
  val (count, from, to) = move
  val buffer = Stack.empty[Char]
  for (i <- 0 until count) do {
    buffer.push(state(from).pop())
  }
  for (i <- 0 until count) do {
    state(to).push(buffer.pop())
  }
  assert(buffer.isEmpty)
}

def execute(executor: Mover)(path: String) = {
  val (state, moves) = parseInput(path)
  for (move <- moves) do {
    executor(state, move)
  }

  state.map(_.top).toList.mkString("")
}
val part1 = execute(executeMove1)
val part2 = execute(executeMove2)


assert(part1("tests") == "CMZ")
assert(part2("tests") == "MCD")

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")