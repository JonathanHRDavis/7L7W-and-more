/*
//	Note:
//	This is the file to execute in the command prompt/terminal. ("day3problem1_3.io")
//	A separate file for input must be used due to changing the Operator Table. ("day3problem1_3_input.io")
//	Steve Dekorte comments on this here: https://github.com/stevedekorte/io/issues/259
*/

OperatorTable addAssignOperator(":", "atPutNumber")

curlyBrackets := method(
	r := Map clone
	call message arguments foreach(arg,
		r doMessage(arg)
	)
	r
)

Map atPutNumber := method(
	self atPut(
		call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
		call evalArgAt(1)
	)
)


Handler := Object clone
Handler forward := method(
	nil
)


Builder := Object clone
Builder indention := 0
Builder forward := method(
	for(i,1,indention, "    " print)
	
	tag := call message name
	write ("<", tag)

	args := call message arguments 
	numArgs := args size
	firstArg := args at(0)

	content := Handler doMessage(firstArg)
	numMaps := 0
	
	if(content type == "Map",
		for(i, 0, numArgs-1,
			content = Handler doMessage(args at(i))
			if(content type == "Map",
				numMaps = numMaps + 1
				key := content keys at(0)
				value := content values at(0)
				write(" ", key, "=", value)
			)
		)
		indention = indention - numMaps
		writeln(">"),
	//else
		writeln(">")
	)

	
	call message arguments foreach(arg,
		indention = indention + 1
		content = self doMessage(arg)
		
		if(content type == "Sequence",
			for(i,1,indention, "    " print)
			writeln(content)
			indention = indention - 1
		)		
	)
	
	for(i,1,indention, "    " print)
	writeln("</", call message name, ">")
	indention = indention - 1
	
)



doRelativeFile("day3problem1_3_input.io")