/*
(C) 2009-2010 Bill Burdick

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

object Test {
	implicit def stringToWordCounter(s : String) = new {
		def wordCounts = s.split(Array(' ', '.', '?')).map(_ trim).filter(! _.isEmpty).size
	}
	def main(args: Array[String]) {
		println("Hello Extension Methods".wordCounts)
	}
}
