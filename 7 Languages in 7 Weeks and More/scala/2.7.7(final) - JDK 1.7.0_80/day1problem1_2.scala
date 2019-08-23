import scala.collection.mutable.ArrayBuffer

class gameBoard 
{
	val guide = List("1","2","3","4","5","6","7","8","9")
	
	//var locations = List("_","_","_","_","_","_","_","_","_")
	var locations = new ArrayBuffer[String]
	locations ++= List("_","_","_","_","_","_","_","_","_")
	
	var currentPlayer = "X"
	var deadState = false
	var gameOver = false
	
	def isDeadState(locs: List[String]) = deadState = !locs.contains("_")
	
	def isWinningState(locs: List[String]) = {
												//First test the horizontal winning states
												if (((locs(0) != "_") && (((locs(0) == locs(1)) && (locs(1) == locs(2)))))
												|| ((locs(3) != "_") && ((locs(3) == locs(4)) && (locs(4) == locs(5))))
												|| ((locs(6) != "_") && ((locs(6) == locs(7)) && (locs(7) == locs(8))))
												
												//Then test for the vertical winning states
												|| ((locs(0) != "_") && ((locs(0) == locs(3)) && (locs(3) == locs(6))))
												|| ((locs(2) != "_") && ((locs(2) == locs(5)) && (locs(5) == locs(8))))
												|| ((locs(1) != "_") && ((locs(1) == locs(4)) && (locs(4) == locs(7))))
												
												//Finally test for the diagonal winning states
												|| ((locs(0) != "_") && ((locs(0) == locs(4)) && (locs(4) == locs(8))))
												|| ((locs(2) != "_") && ((locs(2) == locs(4)) && (locs(4) == locs(6)))))
													gameOver = true
										     }
	
	def printB(locs: List[String]) = { 
									   println (locs(0) + " " + locs(1) + " " + locs(2))
									   println (locs(3) + " " + locs(4) + " " + locs(5))
									   println (locs(6) + " " + locs(7) + " " + locs(8))
									 }
							
	def printGuide = { 
						println("Locations on the board correspond to the following numbers:") 
						printB(guide)
					 }
		
	//def printBoard = printB(locations)
	def printBoard = printB(locations.toList)
	
	def switchPlayer = {
							if(currentPlayer == "X")
								currentPlayer = "O"
							else
								currentPlayer = "X"
					   }
					   
	def isValidMove(move: Int) = {
									if(move < 0 || move > 8)
										false
									else if(locations(move) != "_")
										false
									else
										true
								 }
	
	def takeTurn = {
						printBoard
						println("")
						println("It's " + currentPlayer + "'s turn.")
						print("Enter the number corresponding to your move: ")
						var validMove = false
						
						while(!validMove)
						{
							//val move = scala.io.StdIn.readLine().toInt
							val move = Console.readLine().toInt
							
							if (isValidMove(move - 1))
							{
								//locations = locations.updated((move - 1), currentPlayer)
								locations.update((move - 1), currentPlayer)
								validMove = true
							}
							else
								print("Invalid move! Please enter a different number: ")
							
						}
						
						switchPlayer
				   }
			   
	def playGame = {
						printGuide
						
						while(!gameOver && !deadState)
						{
							println("")
							takeTurn
							//isDeadState(locations)
							//isWinningState(locations)
							isDeadState(locations.toList)
							isWinningState(locations.toList)
						}
						
						println("")
						printBoard
						
						if(gameOver)
						{
							switchPlayer
							println("Game Over. " + currentPlayer + " wins!")
						}
						else
							println("Game Over. There is no winner...")
				   }
}

val myBoard = new gameBoard
myBoard.playGame
