//> using lib "com.lihaoyi::os-lib:0.9.0"

def parseInput: List[Int] = {
  val in = os.read(os.pwd / "inputs" / "day_01.txt")
  in.split("\n\n").map(bag => bag.split("\n").map(Integer.parseInt).foldLeft(0)(_ + _)).toList
}

def part1 = {
  println(s"Part 1: ${parseInput.max}")
}

def part2 = {
  println(s"Part 2: ${parseInput.sorted(summon[Ordering[Int]].reverse).take(3).reduce(_ + _)}")
}

part1
part2
