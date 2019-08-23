import scala.collection.mutable.HashMap
import scala.io.Source

class Sentence(sentence: String)
{
	val text = sentence
	override def toString = text
}

trait Censor 
{ 
	val curseWords = new HashMap[String, String]
	
	def loadCurseWords(file: String) = {
											val lines = Source.fromFile(file).getLines
											lines.foreach(line => {
												val tokens = line.split("->")
												curseWords += tokens(0) -> tokens(1)
											})
									   }
	
	def censor(string: String) = curseWords.foldLeft(string)((curr, curse) => curr.replace(curse._1, curse._2))
}

class censoredSentence(sentence: String) extends Sentence(sentence) with Censor
{
	def censorSentence = censor(text)
}
 
val CS = new censoredSentence("Oh Shoot, I Hate Those Darn Kids.")
CS.loadCurseWords("curses.txt")

println("Original Sentence:")
println(CS)

println("")

println("Censored Sentence:")
println(CS.censorSentence)