val testList = List("One", "Two", "Three", "Four", "Five") //3 + 3 + 5 + 4 + 4 = 19 
def sizeOfStringList(list: List[String]) = list.foldLeft(0)((sum, string) => sum + string.size)
println("Size of " + testList + " is: " + sizeOfStringList(testList))