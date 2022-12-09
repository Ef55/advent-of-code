//> using lib "com.lihaoyi::os-lib:0.9.0"

import scala.collection.mutable.{Stack, Buffer}

sealed trait FS {
  override def toString: String = toStrings.mkString("\n")

  def toStrings: List[String] = this match {
    case File(name, size) => List(f"- ${name} (file, size=${size})")
    case Directory(name, children) => f"- ${name} (dir, size=${size})" :: children.flatMap(_.toStrings).map(line => "  " + line)
  }

  lazy val size: Int = this match {
    case File(_, s) => s 
    case Directory(_, children) => children.map(_.size).foldLeft(0)(_ + _)
  }
}
case class File(name: String, sz: Int) extends FS 
case class Directory(name: String, children: List[FS]) extends FS

given (using io: Ordering[Int]): Ordering[FS] with {
  override def compare(lhs: FS, rhs: FS): Int = {
    io.compare(lhs.size, rhs.size)
  }
}

// Here we completely abuse the schema that the input follows
def parseInput(path: String): FS = {
  val stack = Stack[(String, Buffer[FS])]()
  val blocks = os.read(os.pwd / path / "day_07.txt").split("\\$").tail
  
  def goUp = {
    val (name, subs) = stack.pop
    val dir = Directory(name, subs.toList)
    stack.top._2.append(dir)
  }

  blocks.map(block => {
    block.split("\n").map(_.trim).toList match {
      case s"cd .." :: Nil => goUp
      case s"cd ${to}" :: Nil => stack.push((to, Buffer.empty))
      case s"ls" :: res => res.filter(!_.startsWith("dir")).foreach{ case s"${size} ${name}" => stack.top._2.append(File(name, Integer.parseInt(size))) }
      case s => throw IllegalArgumentException(s"Wrong command format: ${s.mkString(" ")}")
    }
  })

  while (stack.length != 1) {
    goUp
  }
  val (name, subs) = stack.pop
  assert(name == "/")
  Directory(name, subs.toList)
}

def dirs(root: FS): Set[Directory] = {
  root match {
    case d@Directory(_, subdirs) => 
      Set(d) ++ subdirs.flatMap(dirs)
    case _ => Set()
  }
}

def part1(path: String) = {
  val fs = parseInput(path)
  dirs(fs).filter(_.size <= 100000).foldLeft(0)(_ + _.size)
}

def part2(path: String) = {
  val diskCapacity = 70000000
  val requiredSpace = 30000000
  val fs = parseInput(path)

  val missingSpace = requiredSpace - (diskCapacity - fs.size)
  dirs(fs).filter(_.size >= missingSpace).toList.sorted.head.size
}

assert(part1("tests") == 95437)
assert(part2("tests") == 24933642)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")