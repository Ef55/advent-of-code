//> using lib "com.lihaoyi::os-lib:0.9.0"
//> using lib "org.scala-lang.modules::scala-parser-combinators:2.1.1"

import scala.util.parsing.combinator._

trait Packet {
  def toSequence: Sequence = this match {
    case s: Sequence => s
    case _ => Sequence(List(this))
  }
}
case class Number(i: Int) extends Packet
case class Sequence(ls: List[Packet]) extends Packet

given packetOrdering: Ordering[Packet] with {
  def compare(lhs: Packet, rhs: Packet): Int = {
    val oi = summon[Ordering[Int]]
    val olp: Ordering[List[Packet]] = math.Ordering.Implicits.seqOrdering(packetOrdering)
    (lhs, rhs) match {
      case (Number(i), Number(j)) => oi.compare(i, j)
      case (_, _) => 
        val Sequence(ls) = lhs.toSequence
        val Sequence(rs) = rhs.toSequence
        olp.compare(ls, rs)
    }
  }
}

def parseInput(path: String): List[(Packet, Packet)] = {
  object Parser extends RegexParsers {
    val number: Parser[Packet] = """[0-9]+""".r ^^ { str => Number(Integer.parseInt(str)) }
    val sequence: Parser[Packet] = ("\\[".r ~> repsep(any, ",".r) <~ "\\]".r) ^^ { str => Sequence(str.toList) }
    val any: Parser[Packet] = number | sequence

    def parseSequence(str: String): Packet = {
      val Success(res, _) = parse(sequence, str) : @unchecked
      res
    }
  }

  val in = os.read(os.pwd / path / "day_13.txt").split("\n\n").toList
  in.map(str => {
    val lhs :: rhs :: Nil = str.split("\n").map(Parser.parseSequence).toList : @unchecked
    (lhs, rhs)
  })
}

def part1(path: String)(using ord: Ordering[Packet]) = {
  parseInput(path).zipWithIndex.collect{
    case ((p1, p2), i) if ord.compare(p1, p2) < 0 => i+1
  }.foldLeft(0)(_ + _)
}

def part2(path: String) = {
  val div1 = Sequence(List(Sequence(List(Number(2)))))
  val div2 = Sequence(List(Sequence(List(Number(6)))))
  val packets: List[Packet] = div1 :: div2 :: parseInput(path).flatMap( (l, r) => List(l, r) ) 

  val sorted = packets.sorted
  (sorted.indexOf(div1)+1) * (sorted.indexOf(div2)+1)
}

assert(part1("tests") == 13)
assert(part2("tests") == 140)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")