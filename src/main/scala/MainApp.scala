object HanoiSetup {
  val sleep = 500
  val padLen = 6
  val n = 5
}

object MainApp extends App {
  val s = Hanoi.initState(HanoiSetup.n)
  Hanoi.replaceTower(0, 2, s)
}

case class HanoiState(towers: List[List[Int]]){
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
}

object Hanoi{
  def initState(n: Int):HanoiState ={
    HanoiState(
      List((1 to n).toList.reverse, List[Int](), List[Int]())
    )
  }

  def render(s:HanoiState):Unit={
    print("\r" + s.towers.foldLeft("")((accu, tower) => accu + printList(tower)))
  }
  def printList(l: List[Int]) : String ={
    l.foldLeft("")((accu,num)=>accu+num).padTo(HanoiSetup.padLen, ' ')
  }

  def replaceTower(from: Int, to: Int, s: HanoiState): Unit ={
    val indices: Seq[Int] = s.towers(from).indices
    val replaced = indices.foldLeft(s){ (state, idx) => replaceDisc(s.towers(from)(idx), to, state)}
    render(replaced)
  }

  def replaceDisc(size: Int, toTowerIdx: Int, s: HanoiState): HanoiState = {
    val fromTower = s.towers.indexWhere(_.contains(size))
    val subIdx = s.towers(fromTower).indexWhere(_ == size)
    replaceDisc(fromTower, subIdx, toTowerIdx, s)
  }

  def replaceDisc(fromTowerIdx: Int, fromTowerSubIdx: Int, toTowerIdx: Int, s: HanoiState) :HanoiState ={
    def moveToCollidingTower(currSize: Int) = {
      // target has to be prepared
      // find biggest disc idx which is smaller than currSize
      val collIdx = s.towers(toTowerIdx).indexWhere(_ < currSize)
      val clearTarget = replaceDisc(toTowerIdx, collIdx, s.restTowerIdx(fromTowerIdx, toTowerIdx), s)
      val movedToTarget = replaceDisc(fromTowerIdx, fromTowerSubIdx, toTowerIdx, clearTarget)
      movedToTarget
    }

    def moveUpperToOther = {
      // not topmost, move upper disc to other tower
      val clearSource = replaceDisc(fromTowerIdx, fromTowerSubIdx + 1, s.restTowerIdx(fromTowerIdx, toTowerIdx), s)
      replaceDisc(fromTowerIdx, fromTowerSubIdx, toTowerIdx, clearSource)
    }

    def executeMove = {
      // move
      Thread.sleep(HanoiSetup.sleep)
      render(s)
      s.move(fromTowerIdx, toTowerIdx)
    }

    def isTopmost = s.towers(fromTowerIdx).size == fromTowerSubIdx + 1

    if(isTopmost) {
      // topmost disc
      val currSize = s.towers(fromTowerIdx)(fromTowerSubIdx)
      if (s.towers(toTowerIdx).lastOption.getOrElse(Int.MaxValue) > currSize) {
        executeMove
      } else {
        moveToCollidingTower(currSize)
      }
    }
    else{
      moveUpperToOther
    }
  }
}
