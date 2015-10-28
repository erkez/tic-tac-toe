import TicTacToe._

trait Versus extends GameRunner {
  override def startGame: Player = {
    def iterate(state: State, nextPlayer: Player): Player = {
      if (state.availablePositions.isEmpty) {
        return NoPlayer
      }

      println(state.board)
      val newState = state play getHumanMove(state, nextPlayer)
      if (newState.isEndGame) nextPlayer else iterate(newState, nextPlayer.opponent)
    }

    iterate(initialState, X)
  }
}
