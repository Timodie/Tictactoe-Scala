class Game( turn:Player,board:Matrix[Option[Player]]) extends GameLike[Game] {

	

  def isFinished(): Boolean = {
  	//println(hasPlayerWon(O))
  	hasPlayerWon(X) || hasPlayerWon(O)||isDraw()
    }

  	def hasPlayerWon(player :Player ) :Boolean = {
  		//print("im here")
		val row =board.rows.exists(x=>x.forall(cell => cell == Some(player)))
		val cols = board.cols.exists(x => x.forall(cell => cell == Some(player)))
		val maindiag = board.mainDiagonal.forall(cell => cell == Some(player))
		val antidiag = board.antiDiagonal.forall(cell => cell == Some(player))
		//print(antidiag+" " )
		row ||cols||maindiag||antidiag
	}

	def isDraw():Boolean = {
		board.toMap.toList.length == board.rows.length*board.cols.length && 
		hasPlayerWon(X)==false && hasPlayerWon(O)==false
	}
  /* Assume that isFinished is true */
  def getWinner(): Option[Player] ={
  	if(hasPlayerWon(X)){
  		Some(X)
  	}
  	else if(hasPlayerWon(O)){
  	Some(O)
  }
  else{
  	None
  }
}
def getBoard():Matrix[Option[Player]] =board
	def getPlayer():Player =turn


  def nextBoards(): List[Game] ={
  /*traverse board and find empty cells
	empty spots are going to have value none
	change value none to X or O depending on who's turn 
	for all might not work,consider mapping
	*helper takes a list of games and returnlists of games
	each row,you 
	*/
	if(isFinished()){
		List()
	}
	else{
		board.toList(nextBoardhelp).filter(x => (x.getBoard.equals(board))==false)
	}
}

	def nextBoardhelp(a:Int,b:Int,value:Option[Player]):Game = value match {
	case None =>
		if(turn == X)
			new  Game(O,board.set(a,b,Some(X))) 
		else{
			new Game(X,board.set(a,b,Some(O)))
		}
	case Some(_) => new Game(turn,board)
}

}

object Solution extends MinimaxLike {

  type T = Game // T is an "abstract type member" of MinimaxLike
		
  def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game ={
	 new Game(turn,Matrix.fromMap(dim,None,board.mapValues(p => Some(p))))
	 	
	 	
  }


   def minimax(board: Game): Option[Player] ={
   		board.getPlayer match {
   		case O => minimaxOHelp(board)

 		case X => minimaxXHelp(board)
   			
   		
   		
   		

//   	if(board.getPlayer() == Some(X)){
//   		if(board.getWinner() == Some(O)) {
//   		Some(O)
//   	}
//   	else if(board.isDraw()){
//   		None
//   	}
//    if(board.nextBoards().map(minimax).contains(Some(X))){
//   	Some(X)
// 	}
// 	else if(board.nextBoards().map(minimax).contains(None)){
// 		None
// 	}
// 	else {
// 		Some(O)
// 	}
// }
// if(board.getPlayer() == Some(O)){
//   		if(board.getWinner() == X) {
//   		Some(X)
//   	}
//   	else if(board.isDraw()){
//   		None
//   	}
//    if(board.nextBoards().map(minimax).contains(Some(O))){
//   	Some(O)
// 	}
// 	else if(board.nextBoards().map(minimax).contains(None)){
// 		None
// 	}
// 	else {
// 		Some(X)
// 	}
// }
	

	}
}
	def minimaxOHelp(board :Game) : Option[Player] ={

	board.getWinner() match{
   		case Some(X) => Some(X)
   		case _ => board.nextBoards().map(minimax).contains(Some(O)) match {
   		case true => Some(O)
   		case false => board.nextBoards().map(minimax).contains(Some(X)) match {
   			case true  =>  Some(X)
   			case false => board.isDraw match {
   			case true => None
   			case _ => None
   		}
   						
   		}
   					 
   		}
   				
   		}	
	}
  	
  	def minimaxXHelp(board :Game) : Option[Player] ={
  		board.getWinner() match {
   	case Some(O) => Some(O)

   	case _ => board.nextBoards().map(minimax).contains(Some(X)) match {
   					//check draw here
   	case true => Some(X)
   	case false => board.nextBoards().map(minimax).contains(Some(O)) match {
   	case true  => Some(O)
   	case false => board.isDraw match {
   	case true =>None 
   	case _ => None
   		}
   						
   	}
   					
	}
   				 
   }

  	}
  	
 } 	
