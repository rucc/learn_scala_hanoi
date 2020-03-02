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
    var mutatedState = s
    for(i <- s.towers(from).indices) {
      mutatedState = replaceDisc(s.towers(from)(i), to, mutatedState)
    }
    render(mutatedState)
  }

  def replaceDisc(size: Int, toTowerIdx: Int, s: HanoiState): HanoiState = {
    val fromTower = s.towers.indexWhere(_.contains(size))
    val subIdx = s.towers(fromTower).indexWhere(_ == size)
    replaceDisc(fromTower, subIdx, toTowerIdx, s)
  }

  def replaceDisc(fromTowerIdx: Int, fromTowerSubIdx: Int, toTowerIdx: Int, s: HanoiState) :HanoiState ={
    var mutatedState = s
    if(mutatedState.towers(fromTowerIdx).size == fromTowerSubIdx + 1) {
      // topmost disc
      val currSize = s.towers(fromTowerIdx)(fromTowerSubIdx)
      if (mutatedState.towers(toTowerIdx).lastOption.getOrElse(Int.MaxValue) > currSize) {
        // move
        Thread.sleep(HanoiSetup.sleep)
        render(s)
        return mutatedState.move(fromTowerIdx, toTowerIdx)
      } else {
        // target has to be prepared
        // find biggest disc idx which is smaller than currSize
        val collIdx = mutatedState.towers(toTowerIdx).indexWhere(_ < currSize)
        mutatedState = replaceDisc(toTowerIdx, collIdx, mutatedState.restTowerIdx(fromTowerIdx, toTowerIdx), mutatedState)
        mutatedState = replaceDisc(fromTowerIdx, fromTowerSubIdx, toTowerIdx, mutatedState)
      }
    }
    else{
      // not topmost, move upper disc to other tower
      mutatedState = replaceDisc(fromTowerIdx, fromTowerSubIdx + 1, s.restTowerIdx(fromTowerIdx, toTowerIdx), mutatedState)
      mutatedState = replaceDisc(fromTowerIdx, fromTowerSubIdx, toTowerIdx, mutatedState)
    }
    mutatedState
  }
}
