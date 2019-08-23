List myAverage := method(
	sum := 0
	empty := true
	foreach(element,
		if(element type != "Number",
			Exception raise("List has non-numeric value of: \"" .. element .. "\""),
		//else
			empty = false
			sum = sum + element
		)
	)
	if(empty,nil,sum/size)
)

"Taking average of list(1,2,3,4,5)..." println
myList := list(1,2,3,4,5)
("Average is: " .. myList myAverage) println //Should be 3

"" println

"Taking average of list()..." println
myList := list()
("Average is: " .. myList myAverage) println //Should be nil

"" println

"Taking average of list(1,2,\"three\",4,5)..." println
myList = list(1,2,"three",4,5)
("Average is: " .. myList myAverage) println //Should throw exception