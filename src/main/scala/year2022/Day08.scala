package year2022

object Day08 extends App {

  private def isVisible(grid: Seq[Seq[Int]], x: Int, y: Int): Boolean = {

    val value = grid(y)(x)

    val column = grid.map(row => row(x))
    val row = grid(y)

    val leftTrees = row.slice(0, x)
    val rightTrees = row.slice(x + 1, row.length)
    val topTrees = column.slice(0, y)
    val botTrees = column.slice(y + 1, column.length)

    val allColls = Seq(leftTrees, rightTrees, topTrees, botTrees)

    val isEdge = allColls.exists(coll => coll.isEmpty)

    val visibleFrom =
      allColls
        .map(coll => coll.forall(collValue => collValue < value))
        .find(_ == true)

    val isVisible = isEdge || visibleFrom.isDefined
    isVisible

  }

  def sol1(input: String): Int = {
    val parsed = input.split("\n").map(_.split("").map(_.toInt).toSeq).toSeq

    parsed.zipWithIndex
      .flatMap { case (row, y) =>
        row.zipWithIndex.map { case (_, x) =>
          isVisible(parsed, x, y)
        }
      }
      .count(_ == true)

  }

  private def scenicScore(grid: Seq[Seq[Int]], x: Int, y: Int): Int = {

    val value = grid(y)(x)

    val column = grid.map(row => row(x))
    val row = grid(y)

    val topTrees = column.slice(0, y).reverse
    val leftTrees = row.slice(0, x).reverse
    val rightTrees = row.slice(x + 1, row.length)
    val botTrees = column.slice(y + 1, column.length)

    val allColls =
      Seq(topTrees, leftTrees, rightTrees, botTrees).map(coll => {
        val allLower = coll.forall(v => v < value)
        if (allLower) {
          coll.length
        } else {
          coll.zipWithIndex
            .find { case (v, _) => v >= value }
            .map { case (_, index) =>
              index + 1
            }
            .getOrElse(0)
        }
      })

    val rv = allColls.product

    rv

  }

  def sol2(input: String): Int = {
    val parsed = input.split("\n").map(_.split("").map(_.toInt).toSeq).toSeq

    val rv = parsed.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (_, x) =>
        scenicScore(parsed, x, y)
      }
    }.max

    rv

  }

}
