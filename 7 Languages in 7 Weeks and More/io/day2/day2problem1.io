"Enter the nth number for the Fibonacci sequence" println

nth := (File standardInput readLine) asNumber

"" println

"Using a for loop:" println
prev_num := 0
curr_num := 1
"0" println

if(nth == 0, curr_num = 0,
	"1" println
	for(i, 2, nth, (
		sum := prev_num + curr_num
		sum println
		prev_num = curr_num
		curr_num = sum
	)),
false)
("The fibonacci number in the " .. nth .. " position is " .. curr_num) println

"" println

"Using recursion:" println
fibonacci := Object clone
fibonacci recursive := method(number,
	if(number == 0, 0,
		if(number > 2,
			(fibonacci recursive(number - 1) + fibonacci recursive(number - 2)),
			1
		)
	)
)

("The fibonacci number in the " .. nth .. " position is " .. fibonacci recursive(nth) ) println
