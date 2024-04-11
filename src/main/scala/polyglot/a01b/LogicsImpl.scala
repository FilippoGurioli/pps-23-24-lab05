package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.Sequences.Sequence
import util.Sequences.Sequence.Cons
import scala.jdk.javaapi.OptionConverters
import scala.util.Random

trait Cell:
  def x: Int
  def y: Int

object Cell:
  private case class CellImpl(x: Int, y: Int) extends Cell
  def apply(x: Int, y: Int): Cell = new CellImpl(x, y)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  var bombCounter = mines;
  var bombs = Sequence.empty[Cell]
  var selected = Sequence.empty[Cell]
  while bombCounter != 0 do
    val x = Random.nextInt(size)
    val y = Random.nextInt(size)
    if bombs.find(_ == Cell(x, y)).isEmpty then 
      bombs = Cons(Cell(x,y), bombs)
      bombCounter -= 1

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if bombs.contains(Cell(x,y)) then
      OptionToOptional(ScalaOptional.Just(neighbours(x, y)))
    else 
      OptionToOptional(ScalaOptional.Empty())

  private def neighbours(x: Int, y: Int): Int =
    var ret = 0
    for i <- x - 1 to x + 1 do
      for j <- y - 1 to y + 1 do
        if !bombs.contains(Cell(i, j)) then ret += 1
    ret
    
  private def size[E](seq: Sequence[E]): Int = 
    if seq.head.isEmpty then 0
    else 1 + size(seq.filter(_ != seq.head))

  def won = size(bombs) + size(selected) == size * size