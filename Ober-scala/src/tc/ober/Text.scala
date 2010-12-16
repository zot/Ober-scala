/*
(C) 2009-2010 Bill Burdick

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

class ArgMatcher(txt: String, var stopAtEol: Boolean = true, var viewer: ScalaViewer[_] = null) extends Iterator[String] {
	val tokenPattern = """\{|}|\n|\\"|[-a-zA-Z0-9_<>|!.:$/]+(?=[^-a-zA-Z0-9_<>|!.:$/]|$)|(?:"((?:[^"]|\n|\\")*)")""".r 
	val matcher = tokenPattern.findAllIn(txt)
	var curlyLevel = 0
	var nextToken: Option[(String, Int, Int, Boolean)] = getNext
	var start = -1
	var end = -1
	var eol = false

	def hasNext = {
		nextToken.isDefined
	}
	def next = {
		val Some((str, nextStart, nextEnd, nextEol)) = nextToken

		start = nextStart
		end = nextEnd
		eol = nextEol
		nextToken = getNext
		if (nextToken.isDefined) {
			val Some((_, ns, _, _)) = nextToken
			if (end == ns) str + next
			else str
		} else {
			str
		}
	}
	def nextOpt = if (hasNext) Some(next) else None
	def getNext: Option[(String, Int, Int, Boolean)] = {
		if (!matcher.hasNext) None
		else {
			val lex = matcher.next
			if (matcher.group(1) != null) Some((matcher.group(1), matcher.start, matcher.end, false))
				else lex match {
				case "\n" =>
					if (stopAtEol) None
					else Some(lex, matcher.start, matcher.end, true)
				case "{" =>
					val oldStart = matcher.start
					val start = matcher.end
					val (success, end) = parseCurlies
				
					if (success) Some((Ober.eval(txt.slice(start, end), viewer).getOrElse("Error: " + Ober.interpError).toString, oldStart, matcher.end, false)) else None
				case "}" =>
					error("UNBALANCED CURLIES")
					None
				case other => Some((other, matcher.start, matcher.end, false))
			}
		}
	}
	def parseCurlies: (Boolean, Int) = {
		var last = ""

		while (last != "}" && matcher.hasNext) {
			last = matcher.next
			if (last == "{") parseCurlies
		}
		if (last == "}") (true, matcher.start)
		else {
			error("Not enough close curlies in arguments")
			(false, 0)
		}
	}
	def error(str: String) {
		if (viewer != null) viewer.errorFromProcess(str+"\n")
		else println(str)
	}
}
object ArgMatcher {
	def apply(txt: String, stopAtEol: Boolean = true, viewer: ScalaViewer[_] = null) = new ArgMatcher(txt, stopAtEol, viewer)
}
object Text {
	def load(script: String) {
		val m = ArgMatcher(script, false)

		()
	}
}
