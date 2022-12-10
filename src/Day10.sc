//> using lib "com.lihaoyi::os-lib:0.9.0"

trait Instruction(val cycles: Int) {}
case class Addx(imm: Int) extends Instruction(2)
case object Noop extends Instruction(1)

def parseInput(path: String, maybeId: Option[Int]): List[Instruction] = {
  val suffix = maybeId.map(i => s"_${i}").getOrElse("")
  os.read(os.pwd / path / s"day_10${suffix}.txt").split("\n")
    .map{
      case "noop" => Noop
      case s"addx ${i}" => Addx(Integer.parseInt(i))
    }
    .toList
}

def execute(instructions: List[Instruction]): List[Int] = {
  def execute(buffer: List[Int], instruction: Instruction): List[Int] = {
    val x = buffer.head
    instruction match {
      case Noop => x :: buffer
      case Addx(imm) => x+imm :: x :: buffer
    }
  }

  instructions.foldLeft(List(1))(execute).reverse
}

def crt(instructions: List[Instruction], width: Int = 40, height: Int = 6): List[List[Char]] = {
  val trace = execute(instructions)
  (for ( (x, i) <- trace.zip(0 until width*height) ) yield {
    val hPos = i % width
    if Set(x-1, x, x+1).contains(hPos) then '#' else '.'
  }).grouped(width).toList
}.ensuring(res =>
  res.length == height &&
  res.forall(_.length == width)  
)

def part1(path: String, maybeId: Option[Int]) = {
  val trace = execute(parseInput(path, maybeId))

  (for (i <- Range(20, 220+1, 40)) yield {
    i * trace(i-1)
  }).foldLeft(0)(_ + _)
}

def part2(path: String, maybeId: Option[Int]): String = {
  crt(parseInput(path,maybeId )).map(_.mkString("")).mkString("\n")
}

assert(execute(parseInput("tests", Option(0))).last == -1)
assert(part1("tests", Option(1)) == 13140)
assert(
  part2("tests", Option(1)) 
  == 
  """##..##..##..##..##..##..##..##..##..##..
    |###...###...###...###...###...###...###.
    |####....####....####....####....####....
    |#####.....#####.....#####.....#####.....
    |######......######......######......####
    |#######.......#######.......#######.....""".stripMargin('|')
)

println(s"Part 1: ${part1("inputs", Option.empty)}")
println(s"Part 2 (You have to copy text, not the ASCII art):\n${part2("inputs", Option.empty)}")