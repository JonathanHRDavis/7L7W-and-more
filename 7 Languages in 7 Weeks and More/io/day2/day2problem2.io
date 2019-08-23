"Dividing by zero now equals zero!!!" println

division := Number getSlot("/")

Number / := method(num,
	if(num == 0, 0,
		self division(num)
	)
)

("100 / 10 is still: " .. (100 / 10)) println
("50 / 5 is still: " .. (50 / 5)) println
("25 / 0 is now: " .. (25 / 0)) println
("0 / 0 is now: " .. (0 / 0)) println