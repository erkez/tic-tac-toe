/**
 * Trait to use with TicTacToe to play versus
 */
trait Versus extends GameRunner {
  override def getTurnMove(state: State, player: TicTacToe.Player): State.Move =
    getHumanMove(state, player)
}
