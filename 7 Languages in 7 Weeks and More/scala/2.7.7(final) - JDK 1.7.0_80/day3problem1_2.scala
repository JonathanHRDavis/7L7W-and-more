/*
* As Day 3 Problem 1 (and bonus) require editing the author's given code,
* I had to downgrade to using Scala 2.7.7(final) and JDK 1.7.0_80.
* This is due to Actors in scala being deprecated and replaced with Akka.
* I found information relating to Scala's Actor migration at this link:
* http://docs.scala-lang.org/overviews/core/actors-migration-guide.html
*/

import scala.io._
import scala.actors._
import Actor._

// START:loader
object PageLoader {
 def getPageSize(url : String) = {
	try {
		Source.fromURL(url).mkString.length
	}
	catch {
		case e: Exception => println("There was some problem trying to access " + url)
		0
	}
 }
 
 def getPage(url: String) = {
	try {
		Source.fromURL(url).mkString
	}
	catch {
		case e: Exception => println("There was some problem trying to access " + url)
		""
	}
 }
}
// END:loader


val regex = "\"http(s)?://[^\"]*".r


// Trying to access Amazon's source yields a 503 error, probably a safeguard against DDoS attacks.
val urls = List("https://www.amazon.com/", 
               "https://www.twitter.com/",
               "http://www.google.com/",
               "http://www.cnn.com/" )
		
// For a fairly quick test, simply use only Google
// val urls = List("http://www.google.com")

		
			   
// START:time
def timeMethod(method: () => Unit) = {
 val start = System.nanoTime
 method()
 val end = System.nanoTime
 println("Method took " + (end - start)/1000000000.0 + " seconds.")
}
// END:time


// START:sequential
def getPageSizeSequentially() = {
 for(url <- urls) {
   println("Size for " + url + ": " + PageLoader.getPageSize(url))
 }
}
// END:sequential


// START:concurrent
def getPageSizeConcurrently() = {
 val caller = self

 for(url <- urls) {
   actor { caller ! (url, PageLoader.getPageSize(url)) }
 }

 for(i <- 1 to urls.size) {
   receive {
     case (url, size) =>
       println("Size for " + url + ": " + size)            
   }
 }
}
// END:concurrent






def getLinks(url: String) = {
   val page = PageLoader.getPage(url)
   regex.findAllIn(page).collect
}

def getPageSizeAndChildren(url: String, links: Seq[String]) = {
	val currPageSize = PageLoader.getPageSize(url)
	links.foldLeft(currPageSize)((sum, link) => {
		try {
			val link_str = link.drop(1)
			//Uncomment to see all of the detected links as their sizes are calculated
			//println(link_str)
			sum + PageLoader.getPageSize(link_str)
		}
		catch {
			case e: Exception => println("There was some problem trying to access " + link.drop(1))
			sum + 0
		}
	})
}


def getPagesSizeSequentially() = {
 for(url <- urls) {
   
   val links = getLinks(url)
   
   println("\nNumber of links on " + url + " is: " + links.size)
   
   val allPagesSize = getPageSizeAndChildren(url, links)
						
   println("Total size of " + url + " and all child links is: " + allPagesSize + "\n")

 }
}


def getPagesSizeConcurrently() = {
 val caller = self

 for(url <- urls) {
   val links = getLinks(url)
   actor { caller ! (url, links.size, getPageSizeAndChildren(url, links)) }
 }

 for(i <- 1 to urls.size) {
   receive {
     case (url, numLinks, size) => {
	        println("\nNumber of links on " + url + " is: " + numLinks)
			println("Total size of " + url + " and all child links is: " + size + "\n") 
	 }           
   }
 }
}




// Old Tests
/*
// START:script
println("Sequential run:")
timeMethod { getPageSizeSequentially }

println("Concurrent run")
timeMethod { getPageSizeConcurrently }
// END:script
*/



println("Sequential run:")
timeMethod { getPagesSizeSequentially }

println("")

println("Concurrent run")
timeMethod { getPagesSizeConcurrently }

