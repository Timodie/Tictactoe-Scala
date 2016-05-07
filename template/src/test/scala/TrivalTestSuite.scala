import Matrix._
import Solution._
import O._
import X._

class TrivalTestSuite extends org.scalatest.FunSuite{


	val board1 = Map((0,0) -> X)
	val myGame =createGame(X,3,Map((0,1) -> X))
	val game2 = createGame(X,3,Map((0,0)-> X, (2,0) -> X, (1,1) -> O, (2,1) -> O, (1,2)-> O, (2,2)-> X))
	
	val Xwon = game2.nextBoards
	val game3 = createGame(O,3,Map((0,0)-> X, (2,0) -> X, (1,1) -> O, (2,1) -> O, (1,2)-> O, (2,2)-> X))
	val Owon = game3.nextBoards
	test("Solution object must be defined"){
		val obj : MinimaxLike =Solution
	}


	 test("Create game works"){
	assert(myGame.isFinished() == false)
	 }
	 test("getWinner"){
	 	assert(myGame.getWinner == None)
	 }
	 test("X won game"){
	 	assert(Xwon(2).getWinner ==Some(X))
	 }
	 test("O won game"){
	 	assert(Owon(2).getWinner == Some(O))
	 }
	 test("Minimax X wins"){
	 assert(Solution.minimax(game2) == Some(X))
	}
	test("Minimax O Wins"){
		assert(Solution.minimax(game3) == Some(O))
	}

}