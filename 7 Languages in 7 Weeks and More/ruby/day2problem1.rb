array = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

puts "Solution using each:"
array.each 	{|x| if x % 4 == 0
					puts x
				else
					print "#{x}, "
				end
			}

puts " "
puts " "

puts "Solution using each_slice:"
array.each_slice(4) {|x| puts "#{x}"}