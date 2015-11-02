import TicTacToe._

trait Versus extends GameRunner {
  override def startGame: Player = {
    def iterate(state: State, nextPlayer: Player): Player = {
      state.winner match {
        case None =>
          val newState = state play getHumanMove(state, nextPlayer)
          iterate(newState, nextPlayer.opponent)
        case Some(player) =>
          showState(state)
          player
      }
    }

    iterate(initialState, X)
  }
}
