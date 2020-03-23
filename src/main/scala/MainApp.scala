object HanoiEnv {
  val debug = false
}

class HanoiSetup (
  val n:Int = 5,
  val afterMove: HanoiState => Unit = HanoiSetup.afterMove
)
object HanoiSetup{
  def afterMove(hs: HanoiState):Unit = {
    Thread.sleep(400)
    print("\r" + hs.render())
  }
}

object MainApp extends App {
  val setup = new HanoiSetup()
  val hanoi = Hanoi(setup)
  val s = HanoiState.initState(setup.n)
  hanoi.replaceSubTower(0, 0, 2, s)
}

case class HanoiState(towers: List[List[Int]]){
  val padLen:Int = towers.flatten.max
  def canmove(from: Int, to: Int): Tuple2[Boolean, String] = {
    if (from == to) {
      return (true, "no move")
    }
    if (towers(from).isEmpty){
      return (false, "invalid move - empty tower")
    }
    if (towers(to).isEmpty){
      return (true, "")
    }
    if (towers(from).last > towers(to).last){
      return (false, s"cannot place ${towers(from).last} on top of ${towers(to).last}")
    }
    (true, "")
  }

  def move(from: Int, to: Int): HanoiState = {
    val cm = canmove(from, to)
    if (!cm._1){
      throw new Exception(cm._2)
    }
    val updatedtowers = towers.updated(to, towers(to).appended(towers(from).last))
    HanoiState(updatedtowers.updated(from, towers(from).slice(0, towers(from).size - 1)))
  }

  def restTowerIdx(one: Int, other: Int) : Int = {
    towers.indices.filter(num => num != one && num != other).head
  }

  def render():String={
    towers.map(_.mkString("").padTo(padLen, ' ')).mkString("")
  }
}

object HanoiState {
  def initState(n: Int): HanoiState = {
    HanoiState(
      List((1 to n).toList.reverse, List[Int](), List[Int]())
    )
  }
}

case class Hanoi(setup: HanoiSetup)
{
  def replaceSubTower(fromTower: Int, subIdx: Int, toTower: Int, s: HanoiState ):HanoiState = {
    if (s.towers(fromTower).indices.max > subIdx) {
      val rest = s.restTowerIdx(fromTower, toTower)
      var restCnt = s.towers(rest).length
      var replaced = replaceSubTower(fromTower, subIdx + 1, rest, s)
      replaced = executeMove(fromTower, toTower, replaced)
      replaceSubTower(rest, restCnt, toTower, replaced)
    } else {
      executeMove(fromTower, toTower, s)
    }
  }

  def executeMove(fromTower: Int, toTower: Int, hs: HanoiState): HanoiState = {
    val moved = hs.move(fromTower, toTower)
    setup.afterMove(moved)
    moved
  }
}
