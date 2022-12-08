//> using lib "com.lihaoyi::os-lib:0.9.0"

def parseRange(str: String): Range = {
  val start :: end :: Nil = str.split("-").toList.map(Integer.parseInt)
  Range(start, end+1)
}

def parsePair(str: String): (Range, Range) = {
  val l :: r :: Nil = str.split(",").toList.map(parseRange)
  (l, r)
}

def parseInput(path: String): List[(Range, Range)] = {
  os.read(os.pwd / path / "day_04.txt").split("\n").map(parsePair).toList
}

def part1(path: String): Int = {
  parseInput(path).count( (l, r) => {
    l.intersect(r).length == Math.min(l.length, r.length)
  })
}

def part2(path: String) = {
  parseInput(path).count( (l, r) => {
    l.intersect(r).length > 0
  })
}

assert(part1("tests") == 2)
assert(part2("tests") == 4)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")