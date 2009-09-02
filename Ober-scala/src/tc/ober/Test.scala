/*
(C) 2009 Bill Burdick

ar.ober.OberDragWidget

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

object Test {
	var bubba: Int = 3;
	var fred: String = "duh";

	def test() {
		println("duh")
	}
	def main(args: Array[String]) {
		println("test: " + args.toList + " fred: " + new Fred(3))
		System.gc
		println("test: " + args.toList + " fred: " + new Fred(3))
	}
}
case class Fred(f: Int) {
	var t: Int = 10;
	override def toString() = "Fred: " + t;
}
