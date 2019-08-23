sumArray := method(array, 
	oneArray := array flatten
	oneArray sum
)

array := list(list(1,2,3),list(4,5,6),list(7,8,9))

// 1 + 2 + 3 = 6
// 4 + 5 + 6 = 15
// 7 + 8 + 9 = 23
// 6 + 15 + 23 = 45
// Total should be 45


("The total sum of all numbers in the 2D array is: " .. sumArray(array)) println