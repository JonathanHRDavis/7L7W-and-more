#Usage: day2problem3.rb [filename.txt] [phrase_to_search_for]
#I have included a "test.txt" file that I made to test for the occurence of "ABC"

filename = ARGV[0]

File.open(filename).each{ |line|
	if line =~ /#{ARGV[1]}/
		puts "Line #{$.}: #{line}"
	end
}