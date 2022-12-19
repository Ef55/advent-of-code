//> using lib "com.lihaoyi::os-lib:0.9.0"

import scala.collection.mutable.{Queue, Map}

case class Coordinates(x: Int, y: Int) {
  def neighbors = List(Coordinates(x-1, y), Coordinates(x+1, y), Coordinates(x, y-1), Coordinates(x, y+1))
}

def parseInput(path: String): (List[List[Char]], Coordinates, Coordinates) = {
  val in = os.read(os.pwd / path / "day_12.txt").split("\n")
  
  var start = Coordinates(0, 0)
  var end = Coordinates(0, 0)

  val terrain = for (row, y) <- in.zipWithIndex yield {
    for (chr, x) <- row.zipWithIndex yield {
      chr match {
        case 'S' => start = Coordinates(x, y); 'a'
        case 'E' => end = Coordinates(x, y); 'z'
        case _ => chr
      }
    }
  }

  (terrain.map(_.toList).toList, start, end)
}

def solve(terrain: List[List[Char]], start: Coordinates, end: Coordinates): Option[Int] = {
  def inBounds(c: Coordinates) = 0 <= c.x && c.x < terrain(0).length && 0 <= c.y && c.y < terrain.length
  def validMove(from: Coordinates, to: Coordinates) = 
    ( terrain(to.y)(to.x) - terrain(from.y)(from.x) ) <= 1

  val queue = Queue(start)
  val visited = Map(start -> 0)

  while (!visited.contains(end) && !queue.isEmpty) {
    val current = queue.dequeue()

    for neighbor <- current.neighbors if inBounds(neighbor) && !visited.contains(neighbor) && validMove(current, neighbor) do {
      visited += neighbor -> (visited(current)+1)
      queue.enqueue(neighbor)
    }

  }

  visited.get(end)
}

def findAs(terrain: List[List[Char]]): IndexedSeq[Coordinates] = {
  for {
    y <- 0 until terrain.length;
    x <- 0 until terrain(0).length 
    if terrain(y)(x) == 'a'
  } yield {
    Coordinates(x, y)
  }
}

def part1(path: String): Int = {
  (parseInput.andThen(solve).andThen(_.get))(path)
}

def part2(path: String): Int = {
  val (terrain, _, end) = parseInput(path)
  val starts = findAs(terrain)

  starts.map(solve(terrain, _, end)).collect{ case Some(r) => r }.min
}

assert(part1("tests") == 31)
assert(part2("tests") == 29)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")