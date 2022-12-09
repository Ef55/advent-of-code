//> using lib "com.lihaoyi::os-lib:0.9.0"

def parseInput(path: String, modifier: Option[Int]): String = {
  val end = if modifier.isEmpty then "" else s"_${modifier.get}"
  os.read(os.pwd / path / s"day_06${end}.txt")
}

def findMarker(markerSize: Int)(path: String, modifier: Option[Int]): Int = {
  var in = parseInput(path, modifier)
  var i = 0

  while (!in.isEmpty()) {
    val prefix = in.take(markerSize)
    if (prefix.distinct.length() == markerSize) {
      return i + markerSize
    }
    in = in.drop(1)
    i += 1
  }
  throw IllegalArgumentException("Solution not found!")
}
val part1 = findMarker(4)
val part2 = findMarker(14)

val solutions1 = List(7, 5, 6, 10, 11)
val solutions2 = List(19, 23, 23, 29, 26)

for (((s1, s2), i) <- solutions1.zip(solutions2).zipWithIndex) do {
  assert(part1("tests", Some(i)) == s1)
  assert(part2("tests", Some(i)) == s2)
}

println(s"Part 1: ${part1("inputs", Option.empty)}")
println(s"Part 2: ${part2("inputs", Option.empty)}")