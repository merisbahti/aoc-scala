package year2022

object Day07 extends App {
  sealed trait Tree

  case class Dir(name: String, nodes: Seq[Tree]) extends Tree

  case class File(name: String, size: Int) extends Tree

  private def createPath(path: Seq[String], dir: Dir, file: File): Dir = {
    path match {
      case Seq() => dir.copy(nodes = dir.nodes.appended(file))
      case seq =>
        // create OR find folder
        val head = seq.head
        val tail = seq.tail
        val emptyFolder = dir.nodes
          .find {
            case Dir(dirName, _) => dirName == head
            case File(_, _) => false
          }
          .getOrElse(Dir(head, Seq()))
          .asInstanceOf[Dir]
        val finishedFolder = createPath(tail, emptyFolder, file)
        dir.copy(nodes =
          dir.nodes
            .filter {
              case Dir(dirName, _) => dirName != head
              case File(_, _) => true
            }
            .appended(finishedFolder)
        )
    }
  }

  private def getSize(tree: Tree): Int = {
    tree match {
      case Dir(_, nodes) => nodes.map(getSize).sum
      case File(_, size) => size
    }
  }

  def parse(input: String): Tree = {
    val commands = input.split("\\$ ").tail.tail
    var currDir: Seq[String] = Seq()
    var struct: Seq[(Seq[String], File)] = Seq()
    val cdRegex = "cd (.+)".r
    val fileRegex = "(\\d+) (.*)".r
    val dirRegex = "dir (.*)".r
    commands.foreach { command =>
      val split = command.split("\n").toSeq
      val cwdFormatted = currDir
      split match {
        case Seq("ls", nodes@_*) =>
          nodes.foreach {
            case fileRegex(sizeStr, name) =>
              val newEntry = cwdFormatted -> File(
                name,
                sizeStr.toInt
              )
              struct = struct.appended(newEntry)
            case dirRegex(_) =>
          }
        case Seq(cdRegex(dir)) =>
          if (dir == "..") {
            currDir = currDir.reverse.tail.reverse
          } else {
            currDir = currDir.appended(dir)
          }
      }
      split
    }

    struct.foldLeft(Dir("", Seq())) { case (acc, (path, file)) =>
      createPath(path, acc, file)
    }

  }

  def sol1(input: String): Int = {

    def sol1sum(tree: Tree): Int = {
      tree match {
        case Dir(_, nodes) =>
          val size = getSize(tree)
          val childSizes = nodes.map(sol1sum).sum
          childSizes + (if (size < 100000) size else 0)
        case _ => 0
      }

    }

    val fileSystem = parse(input)

    val rv = sol1sum(fileSystem)
    rv

  }

  def sol2(input: String): Int = {

    def sol2sum(tree: Tree, acc: Seq[Int]): Seq[Int] = {
      tree match {
        case Dir(_, nodes) =>
          val size = getSize(tree)
          val childSizes = nodes.foldLeft(acc) { case (a, b) =>
            sol2sum(b, a)
          }
          childSizes.appended(size)

        case _ => acc
      }

    }

    val fileSystem = parse(input)

    val rv = sol2sum(fileSystem, Seq())

    val topDir = rv.max

    val totalSize = 70000000

    val sizeAvailable = totalSize - topDir

    val sizeNeeded = 30000000 - sizeAvailable

    rv.filter(x => x != topDir).filter(_ > sizeNeeded).min

  }

}
