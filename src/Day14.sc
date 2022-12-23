//> using lib "com.lihaoyi::os-lib:0.9.0"
//> using lib "org.scala-lang.modules::scala-parser-combinators:2.1.1"

import scala.util.parsing.combinator._

import scala.collection.immutable.Set

case class Coordinates(x: Int, y: Int) {
  lazy val down = Coordinates(x, y+1)
  lazy val downLeft = Coordinates(x-1, y+1)
  lazy val downRight = Coordinates(x+1, y+1)
}

def undirectedRange(lhs: Int, rhs: Int): Range = {
  Math.min(lhs, rhs) to Math.max(lhs, rhs)
}

def parseInput(path: String): Set[Coordinates] = {
  val in = os.read(os.pwd / path / "day_14.txt")
  var points = Set.empty[Coordinates]

  object Parser extends RegexParsers {
    val integer = """\d+""".r ^^ Integer.parseInt
    val point = integer ~ ("," ~> integer) ^^ { case l~r => Coordinates(l,r) }
    val line = repsep(point, "->".r)

    def apply(in: String) = {
      val Success(res) = parseAll(line, in) : @unchecked
      res
    }
  }

  for (line <- in.split("\n")) do {
    val pointsSeq = Parser(line)
    var start = pointsSeq.head
    for (point <- pointsSeq) do {

      for {
        x <- undirectedRange(start.x, point.x); 
        y <- undirectedRange(start.y, point.y)
      } do {
        points = points + Coordinates(x, y)
      }

      start = point
    }
  }

  points
}

def computeMaxY(points: Set[Coordinates]): Int = {
  points.foldLeft(0)( (a, c) => Math.max(a, c.y))
}

def addFloor(points: Set[Coordinates], y: Int): Set[Coordinates] = {
  val init = points.toSeq.head.x
  val maxY = Math.max(computeMaxY(points), y)
  val minX = points.foldLeft(init)( (a, c) => Math.min(a, c.x)) - maxY - 1
  val maxX = points.foldLeft(init)( (a, c) => Math.max(a, c.x)) + maxY + 1

  var current = points
  for (x <- minX to maxX) do {
    current = current + Coordinates(x, y)
  }

  current
}

def simulate(init: Set[Coordinates]): Set[Coordinates] = {
  val max = computeMaxY(init)

  var current = init
  def display = {
    println(printFrame(current, Coordinates(485, 0), Coordinates(515, 11)))
  }

  val origin = Coordinates(500, 0)
  var pos = origin

  while (pos.y < max && !current.contains(origin)) {
    if (!current.contains(pos.down)) {
      pos = pos.down
    }
    else if (!current.contains(pos.downLeft)) {
      pos = pos.downLeft
    }
    else if (!current.contains(pos.downRight)) {
      pos = pos.downRight
    }
    else {
      current = current + pos
      pos = origin
    }
  }

  current
}

def printFrame(points: Set[Coordinates], lower: Coordinates, upper: Coordinates): String = {
  (for (y <- lower.y to upper.y) yield {
    (for (x <- lower.x to upper.x) yield {
      if (points.contains(Coordinates(x, y))) {
        '#'
      }
      else if (Coordinates(x, y) == Coordinates(500, 0)) {
        '+'
      }
      else {
        '.'
      }
    }).mkString("")
  }).mkString("\n")
}

def part1(path: String) = {
  val init = parseInput(path)
  val end = simulate(init)
  (end &~ init).size
}

def part2(path: String) = {
  var init = parseInput(path)
  val maxY = computeMaxY(init)
  init = addFloor(init, maxY+2)
  val end = simulate(init)
  (end &~ init).size
}

assert(part1("tests") == 24)
assert(part2("tests") == 93)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")