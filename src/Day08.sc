//> using lib "com.lihaoyi::os-lib:0.9.0"

type Grid[T] = List[List[T]]

def toIntGrid(grid: Grid[Boolean]): Grid[Int] = {
  grid.map(row => row.map(if _ then 1 else 0))
}

def pp(grid: Grid[Boolean]): String = {
  toIntGrid(grid).map(_.mkString("")).mkString("\n")
}

def hflip[T](grid: Grid[T]): Grid[T] = {
  grid.map(_.reverse)
}

def parseInput(path: String): Grid[Int] = {
  os.read(os.pwd / path / "day_08.txt").split("\n")
    .map(_.map(c => Integer.parseInt(c.toString)).toList).toList
}

// Absolutely not verbose
def maxHeightRowWiseLeftToRight(grid: Grid[Int]): Grid[Int] = {
  grid.map( row => row.scanLeft(-1)(Math.max).init )
}.ensuring( res =>
  res.length == grid.length &&
  res.zip(grid).forall( (r1, r2) => r1.length == r2.length )
)
def allGridRotations[T]: List[(Grid[T] => Grid[T], Grid[T] => Grid[T])] = List(
  (x => x, x => x),
  (x => hflip(x), x => hflip(x)),
  (x => x.transpose, x => x.transpose),
  (x => hflip(x.transpose), x => hflip(x).transpose)
)

def visibilityMap(grid: Grid[Int]): Grid[Boolean] = {
  val heightMaps = allGridRotations[Int].map( (f, g) => g(maxHeightRowWiseLeftToRight(f(grid))))

  grid.zipWithIndex.map( (row: List[Int], y: Int) =>
    row.zipWithIndex.map( (cell: Int, x: Int) =>
      heightMaps.exists( grid => grid(y)(x) < cell )
    )
  )
}

def count(grid: Grid[Boolean]): Int = {
  toIntGrid(grid).foldLeft(0)(_ + _.foldLeft(0)(_ + _))
}

def viewDistance(grid: Grid[Int], x: Int, y: Int): List[Int] = {
  val coordRot = List[(Int, Int) => (Int, Int)](
    (x, y) => (x, y),
    (x, y) => (grid(0).length - 1 - x, y),
    (x, y) => (y, x),
    (x, y) => (grid.length - 1 - y, x)
  )
  val iters: List[(Grid[Int], (Int, Int))] = allGridRotations[Int].map(_._1(grid)).zip(
    coordRot.map(f => f(x, y))
  )

  val origHeigh = grid(y)(x)

  for ( (grid, (x, y)) <- iters ) yield {
    var d = 0
    var xp = x - 1
    while (xp >= 0 && grid(y)(xp) < origHeigh) {
      d += 1
      xp -= 1
    }
    if (xp >= 0) {
      d += 1
    }
    d
  }
}

def scenicScore(grid: Grid[Int], x: Int, y: Int): Int = {
  viewDistance(grid, x, y).foldLeft(1)(_ * _)
}

def part1(path: String) = {
  val grid = parseInput(path)
  val visibility = visibilityMap(grid)
  count(visibility)
}

def part2(path: String) = {
  val grid = parseInput(path)
  // Inefficient, but does the trick
  (for (y <- 0 until grid.length; x <- 0 until grid(y).length) yield {
    scenicScore(grid, x, y)
  }).max
}

assert(part1("tests") == 21)
assert(part2("tests") == 8)

println(s"Part 1: ${part1("inputs")}")
println(s"Part 2: ${part2("inputs")}")