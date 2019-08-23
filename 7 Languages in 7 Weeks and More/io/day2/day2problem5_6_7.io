///////////////
// Problem 5 //
///////////////
TwoList := Object clone

TwoList init := method(
	self myList := list()
)

TwoList dim := method(x,y,
	for(i,1,y,
		//("Making list number " .. i .. " with " .. x .. " elements") println
		newList := list()
		for(j,1,x,
			newList append("")
		)		
		myList append(newList)
	)
)

TwoList set := method(x,y,value,
	tempList := self myList at(y)
	tempList atPut(x,value)
)

TwoList get := method(x,y,
	tempList := myList at(y)
	tempList at(x)
)

///////////////
// Problem 6 //
///////////////
TwoList transpose := method(
	new_matrix := TwoList clone
	//myList println
	x := myList at(0) size
	y := myList size
	new_matrix dim(y,x)
	for(i,1,y,
		for(k,1,x,
			//("set(" .. i-1 .. "," .. k-1 .. ") to get(" .. k-1 .. "," .. i-1 .. ")" )println
			new_matrix set(i-1,k-1, get(k-1,i-1))
		)
	)
	new_matrix
)

///////////////
// Problem 7 //
///////////////
TwoList write := method(filename,
	myFile := File with(filename) 
	myFile openForUpdating
	myFile write(self serialized()) 
	myFile close
)

TwoList read := method(filename,
	myFile := doFile(filename)
)






matrix1 := TwoList clone
matrix1 dim(3,5)

matrix1 set(0,0,10)
matrix1 set(1,0,20)
matrix1 set(2,0,30)

matrix1 set(0,1,11)
matrix1 set(1,1,21)
matrix1 set(2,1,31)

matrix1 set(0,2,12)
matrix1 set(1,2,22)
matrix1 set(2,2,32)

matrix1 set(0,3,13)
matrix1 set(1,3,23)
matrix1 set(2,3,33)

matrix1 set(0,4,14)
matrix1 set(1,4,24)
matrix1 set(2,4,34)



//I used (2,3) with a value of 999 as the test case below.
//These values can be changed to test for any valid pairing.
testNum1 := 2
testNum2 := 3
matrix1 set(testNum1,testNum2,999)



("Matrix 1 at (" .. testNum1 .. "," .. testNum2 .. ") = " .. matrix1 get(testNum1,testNum2)) println

"" println

"Transposing Matrix 1 into Matrix 2..." println
matrix2 := matrix1 transpose

("Matrix 2 at (" .. testNum2 .. "," .. testNum1 .. ") = " .. matrix2 get(testNum2,testNum1)) println

"" println

"Writing Matrix 1 to file \"matrix.txt\"..." println
matrix1 write("matrix.txt")

"Reading \"matrix.txt\" into Matrix 3..." println
matrix3 := TwoList read("matrix.txt")

("Matrix 3 at (" .. testNum1 .. "," .. testNum2 .. ") = " .. matrix3 get(testNum1,testNum2)) println

