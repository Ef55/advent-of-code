//> using lib "com.lihaoyi::os-lib:0.9.0"

def parseSack(sack: String): (Set[Char], Set[Char]) = {
  val len = sack.length / 2
  val (l, r) = sack.splitAt(len)
  assert(l.length == r.length)
  (Set.from(l), Set.from(r))
}

def parseInput(path: String): List[(Set[Char], Set[Char])] = {
  val in = os.read(os.pwd / path / "day_03.txt")
  in.split("\n").map(parseSack).toList
}

def priority(chr: Char): Int = {
  if (chr.isLower) {
    (chr - 'a') + 1
  }
  else {
    assert(chr.isUpper)
    (chr - 'A') + 27
  }
}

def part1(path: String): Int = {
  parseInput(path).map( (l, r) => l.intersect(r).toList.head )
    .map(priority)
    .foldLeft(0)(_ + _)
}

def part2(path: String): Int = {
  parseInput(path).map( (l, r) => l.union(r) )
    .grouped(3)
    .map( _.reduce( (l, r) => l.intersect(r) ) )
    .map( s => priority(s.toList.head) )
    .foldLeft(0)(_ + _)
}

assert(part1("tests") == 157)
assert(part2("tests") == 70)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")