//> using lib "com.lihaoyi::os-lib:0.9.0"

type Position = (Int, Int)

extension (p: Position) {
  def isNextTo(q: Position): Boolean = {
    Set(p._1, p._1+1, p._1-1).contains(q._1) &&
    Set(p._2, p._2+1, p._2-1).contains(q._2)
  }
}

enum Move {
  case Left, Right, Up, Down
}

def sign(i: Int) = {
  if (i > 0) { 1 }
  else if (i < 0) { -1 }
  else { 0 }
}

def parseInput(path: String, maybeId: Option[Int]): List[Move] = {
  val id = if maybeId.isDefined then s"_${maybeId.get}" else ""
  def parseMove(c: String): Move = c match {
    case "R" => Move.Right
    case "L" => Move.Left
    case "U" => Move.Up
    case "D" => Move.Down
  }

  os.read(os.pwd / path / s"day_09${id}.txt").split("\n").flatMap{
    case s"${move} ${count}" => List.fill(Integer.parseInt(count))(parseMove(move))
  }.toList
}

def checkRope(rope: List[Position]): Boolean = {
  (0 until rope.length-1).forall(i => rope(i).isNextTo(rope(i+1)) )
}

def simulate(rope: List[Position], move: Move): List[Position] = {
  require(checkRope(rope))
  val head = rope(0)
  val newHead = move match {
    case Move.Right => (head._1 + 1, head._2)
    case Move.Left => (head._1 - 1, head._2)
    case Move.Up => (head._1, head._2 + 1)
    case Move.Down => (head._1, head._2 - 1)
  }

  var newRope = newHead :: Nil

  for (i <- 1 until rope.length) do {
    val pred = newRope.head
    val current = rope(i)
    val newCurrent = 
      if (pred.isNextTo(current)) {
        current
      }
      else {
        (current._1 + sign(pred._1 - current._1), current._2 + sign(pred._2 - current._2))
      }
    newRope = newCurrent :: newRope
  }

  newRope.reverse
}.ensuring(checkRope(_))

def simulate(start: Position, knots: Int, moves: List[Move]): List[List[Position]] = {
  require(knots >= 1)
  var ls = List(List.fill(knots)(start))

  for (move <- moves) do {
    ls = simulate(ls.head, move) :: ls
  }

  ls.reverse
}

def tailLocations(knots: Int)(path: String, maybeId: Option[Int]): Int = {
  simulate((0,0), knots, parseInput(path, maybeId)).map(_(knots-1)).distinct.length
}

val part1 = tailLocations(2)
val part2 = tailLocations(10)


assert(part1("tests", Some(0)) == 13)
assert(part2("tests", Some(0)) == 1)
assert(part2("tests", Some(1)) == 36)

println(s"Part 1: ${part1("inputs", Option.empty)}")
println(s"Part 2: ${part2("inputs", Option.empty)}")