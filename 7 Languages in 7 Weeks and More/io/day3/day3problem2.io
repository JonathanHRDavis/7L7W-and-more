squareBrackets := method(
	newList := list()
	args := call message arguments
	args foreach(x,
		newList append(x)
	)
	newList
)

bracketList := [11,22,33,44,55]

bracketList println
("Size: " .. bracketList size) println
("Type: " .. bracketList proto) println