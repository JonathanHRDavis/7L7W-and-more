module ActsAsCsv
  def self.included(base)
    base.extend ClassMethods
  end
  
  module ClassMethods
    def acts_as_csv
      include InstanceMethods
    end
  end
  
  module InstanceMethods   
    def read
      @csv_contents = []
      filename = self.class.to_s.downcase + '.txt'
      file = File.new(filename)
      @headers = file.gets.chomp.split(', ')

      file.each do |row|
        @csv_contents << CsvRow.new(@headers,(row.chomp.split(', ')))
      end
    end
    
    attr_accessor :headers, :csv_contents
    def initialize
      read 
    end
	
	def each
		@csv_contents.each{|csvrow| yield csvrow}
	end
	
	class CsvRow
		def initialize headings, values
			@headings = headings
			@values = values
		end
		
		def method_missing name, *args
			index = @headings.find_index(name.to_s)
			@values[index] unless index == nil
		end
	end
	
  end
end

class RubyCsv  # no inheritance! You can mix it in
  include ActsAsCsv
  acts_as_csv
end

# m = RubyCsv.new
#puts m.headers.inspect
#puts m.csv_contents.inspect

csv = RubyCsv.new 
puts "Should display lions and bears:"
csv.each {|row| puts row.one}

puts ""

puts "Should display tigers and oh_my:"
csv.each {|row| puts row.two}

puts ""

puts "Should display nothing, because there is no \"three\" header:"
csv.each {|row| puts row.three}

