package tc.ober

//this specifies where to put the command, <namespace>.<name>
@CommandName("Ober.example")
object ExampleCommand extends OberCommand {
	//this is the method that Ober calls when someone clicks on the command name
    def runCommand(ctx: SimpleContext) = println("hello")

    // other code that you may need
    println("done")
}
