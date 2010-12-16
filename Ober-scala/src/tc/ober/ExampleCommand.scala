/*
(C) 2009-2010 Bill Burdick

This software is distributed under the terms of the
Artistic License. Read the included file
License.txt for more information.
*/
package tc.ober

//this specifies where to put the command, <namespace>.<name>
//you can override commandName instead if you don't want to use an annotation
@CommandName("Ober.Example")
object ExampleCommand extends OberCommand {
	//this is the method that Ober calls when someone clicks on the command name
    def runCommand(ctx: SimpleContext) = println("hello")

    // other code that you may need
    println("done")
}
