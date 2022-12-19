//> using lib "com.lihaoyi::os-lib:0.9.0"

import scala.collection.mutable.Queue 

case class Monkey(update: Long => Long, divisibleByTest: Int, thenTo: Int, elseTo: Int)

def parseMonkey(str: String): (List[Long], Monkey) = {
  val _ :: startingStr :: operationStr :: testStr :: thennStr :: elzeStr :: Nil = str.split("\n").toList : @unchecked

  val starting = {
    val s"  Starting items: ${list}" = startingStr : @unchecked
    list.split(", ").toList.map(java.lang.Long.parseLong)
  }

  val op = {
    val operandsMap = Map(
      "old" -> ( (x: Long) => x ),
    ).withDefault(y => (x: Long) => java.lang.Long.parseLong(y))
    val operatorsMap = Map(
      "+" -> ( (x: Long, y: Long) => x + y ),
      "*" -> ( (x: Long, y: Long) => x * y ),
    )

    val s"  Operation: new = ${lhs} ${op} ${rhs}" = operationStr : @unchecked
    (old: Long) => operatorsMap(op)(operandsMap(lhs)(old), operandsMap(rhs)(old))
  }

  val test = {
    val s"  Test: divisible by ${v}" = testStr : @unchecked
    Integer.parseInt(v)
  }

  val thenn = {
    val s"    If true: throw to monkey ${v}" = thennStr : @unchecked
    Integer.parseInt(v)
  }

  val elze = {
    val s"    If false: throw to monkey ${v}" = elzeStr : @unchecked
    Integer.parseInt(v)
  }

  (starting, Monkey(op, test, thenn, elze))
}

def parseInput(path: String): List[(List[Long], Monkey)] = {
  val in = os.read(os.pwd / path / "day_11.txt").split("\n\n").toList
  in.map(parseMonkey)
}

def worryHandler(init: List[(List[Long], Monkey)]): List[(List[Long], Monkey)] = {
  val factors = init.map(_._2.divisibleByTest).foldLeft(1)(_ * _)
  init.map( (ls, monkey) => (ls, monkey.copy(update = ( (old: Long) => monkey.update(old) % factors ) )) )
}

def round(in: List[(List[Long], Monkey, Long)], kalm: Int): List[(List[Long], Monkey, Long)] = {
  val state = in.map( (items, monkey, _) => (Queue.from(items), monkey) )
  var counts = List.empty[Long]

  for { (items, monkey) <- state } do {
    var c = 0
    while (!items.isEmpty) {
      val score = items.dequeue()
      val newScore = monkey.update(score) / kalm

      val target = if newScore % monkey.divisibleByTest == 0 then monkey.thenTo else monkey.elseTo
      state(target)._1.enqueue(newScore)
      c += 1
    }
    counts = c :: counts
  }

  assert(counts.length == state.length)
  state.zip(counts.reverse).map{ case ((items, monkey), c) => (items.toList, monkey, c) }
}.ensuring(res => 
  res.forall(p => p._1.forall(_ >= 0))  
)

def monkeyBusiness(trace: List[List[(List[Long], Monkey, Long)]]): Long = {
  val top1 :: top2 :: _ = trace
    .map(_.map(_._3))
    .transpose
    .map(_.tail.scanLeft(0L)(_ + _))
    .transpose
    .last
    .sorted.reverse.toList : @unchecked
  top1 * top2
}

def part1(path: String) = {
  val init = parseInput(path).map{ case (ls, m) => (ls, m, 0L) }
  val trace = (0 until 20)
    .scanLeft(init)( (s, _) => round(s, 3) )
    .toList
  monkeyBusiness(trace)
}

def part2(path: String) = {
  val init = worryHandler(parseInput(path)).map{ case (ls, m) => (ls, m, 0L) }
  val trace = (0 until 10000)
    .scanLeft(init)( (s, _) => round(s, 1) )
    .toList
  monkeyBusiness(trace)
}

assert(part1("tests") == 10605)
assert(part2("tests") == 2713310158L)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")