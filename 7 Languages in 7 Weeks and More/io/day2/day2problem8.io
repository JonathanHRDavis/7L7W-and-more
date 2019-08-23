answer := (Random value * 100) ceil
wrong := true
numGuesses := 0

"Enter your guess for the random number (between 1-100):" println
guess := (File standardInput readLine) asNumber
numGuesses = numGuesses + 1
guessed := false
if(guess == answer,
	wrong = false
)

if(wrong,
	difference := (answer - guess) abs
	if(difference > 50,
		"Woah, you're way off! Try again." println,
	//else
		"You're in the ballpark! Try again." println
	)
	
	guess := (File standardInput readLine) asNumber
	numGuesses = numGuesses + 1
	if(guess == answer,
		wrong = false
	)
	newDifference := (answer - guess) abs
		
	while(wrong and numGuesses < 10,
		if(newDifference < difference,
			"You're hotter! Try again." println,
		//else
			"You're colder... Try again." println	
		)
		difference = newDifference
		guess := (File standardInput readLine) asNumber
		numGuesses = numGuesses + 1
		newDifference := (answer - guess) abs
		if(guess == answer,
			wrong = false
			guessed = true
		)
	)
	if(guessed,
		("Congratulations, you guessed the random number " .. answer .. " in " .. numGuesses .. " guesses!") println,
	//else
		("You were not able to guess the random number " .. answer .. " within 10 guesses... Try again!") println
	),
//else
	"You... Guessed correctly on the first try. Are you psychic!?" println
)



