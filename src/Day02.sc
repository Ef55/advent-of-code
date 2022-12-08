//> using lib "com.lihaoyi::os-lib:0.9.0"

enum Move {
  case Rock, Paper, Scissor
}

def parseOpponentMove(chr: String) = chr match {
  case "A" => Move.Rock
  case "B" => Move.Paper
  case "C" => Move.Scissor
}

def parseSuggestedMove(opp: Move)(chr: String) = chr match {
  case "X" => Move.Rock
  case "Y" => Move.Paper
  case "Z" => Move.Scissor
}

def parseSuggestedOutcome(opp: Move)(chr: String) = chr match {
  case "X" => Move.fromOrdinal( (opp.ordinal - 1 + 3) % 3 )
  case "Y" => opp
  case "Z" => Move.fromOrdinal( (opp.ordinal + 1) % 3 )
}

def parseInput(parseSelf: Move => String => Move): List[(Move, Move)] = {
  val in = os.read(os.pwd / "inputs" / "day_02.txt")
  in.split("\n").map( round => {
    val opp :: self :: Nil = round.split(" ").toList
    val oppMove = parseOpponentMove(opp)
    (oppMove, parseSelf(oppMove)(self))
  }).toList
}

def roundPoints(opponent: Move, self: Move): Int = {
  if ( (opponent.ordinal + 1) % 3 == self.ordinal) {
    // Win
    6
  }
  else if ( opponent.ordinal == self.ordinal) {
    // Draw
    3
  }
  else {
    // Loss
    assert( opponent.ordinal == (self.ordinal + 1) % 3 )
    0
  }
}

def allPoints(rounds: List[(Move, Move)]): Int = {
  rounds.map( (opp, self) => roundPoints(opp, self) + self.ordinal + 1 ).foldLeft(0)(_ + _)
}

println(s"Part 1: ${allPoints(parseInput(parseSuggestedMove))}")
println(s"Part 2: ${allPoints(parseInput(parseSuggestedOutcome))}")

