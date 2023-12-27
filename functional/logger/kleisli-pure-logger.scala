/* We want to create a class which keeps track of the operations in our program.
 * The example below shows a simple implementation of this.
 */
object ImpureLogger {
  var log: String = "Log history so far... "

  def negate(x: Boolean): Boolean = {
    log += "negate has been used! "
    !x
  }

  def main(args: Array[String]): Unit = {
    val result: Boolean = negate(true)
    println(s"Result: $result\n Log state: $log")
  }
}
/* This is of course a simple and good enough example. But we were perhaps a bit too quick to implement a shared mutable state. There are a few clear disadvantages with this approach:
 * 1. In order to tests this method we need to mock the log state so we can have control over our tests.
 * 2. By introducing a new requirement (log the state of the program) we've introduced the idea of a history within the scope of our method.
 * 
 * Other minor "problems":
 * 1. This methods are hard to run in paralell since it may have unexpected results as we change the shared state.
 * 2. Method dependency is not clear from our function signature (the input may be a boolean but we also need access to the log state).
 *
*/

/* So let's make our dependencies more transparent by moving the log state into the function argument
 * instead of using a private state.
 */
object PureLogger {
  def negate(x: Boolean, log: String): (Boolean, String) = {
    (!x, log + "negate has been used! ")
  }

  def main(args: Array[String]): Unit = {
    val result: (Boolean, String) = negate(true, "Log history so far... ")
    println(s"Result: $result")
  }
}
/* This is fine. We already have dependency transparency since we know exactly what we need for our method to run (deterministically).
 * The tests are much easier since this is now a pure function. However we still improve on our current solution. Notice that our methods still
 * has within its scope the idea of a log history which passed as an argument. This is suboptimal since the function should only really do two thing: do is main operation and log the action.
 * It should not have access to whatever log history we currently have. This way, this function can truly be run and tested in isolation.
 */

/* We'll solve this problem in a "monadic" way by introducing a composition process.
 * We'll also introduce some more methods:
 * uppercase -> takes a strings and returns it in uppercase letters.
 * words -> takes a string and returns a list with the characters in the string.
 * Here's our solution:
 */
case class Writer[A](value: A, log: String)
object ComposableLogger {
  def uppercase(s: String): Writer[String] = {
    Writer(s.toUpperCase, "used scream! ")
  }

  def words(s: String): Writer[List[String]] = {
    Writer(s.split("\\s+").toList, "used toWords! ")
  }

  def process(s: String): Writer[List[String]] = {
    val scream: Writer[String] = uppercase(s)
    val toWords: Writer[List[String]] = words(scream.value)

    Writer(toWords.value, s"${scream.log} ${toWords.log}")
  }

  def main(args: Array[String]) = {
    val composition = process("Hello World! This is a great day for scala coding!")

    println(s"Result: ${composition.value}\nLog: ${composition.log}")
  }
}
/* Now we're talking! We not only have dependency transparency but also all our methods can truly be run in isolation.
 * One important aspect of our code is that the log history is created in the process method and not by some shared state to which
 * all our class methods have access to.
 * In the next snippet we're going to create a abstraction for the composition process.
 */
object AbstractComposableLogger {
  def uppercase(s: String): Writer[String] = {
    Writer(s.toUpperCase, "used scream! ")
  }

  def words(s: String): Writer[List[String]] = {
    Writer(s.split("\\s+").toList, "used words! ")
  }
  
  def compose[A, B, C](f: A => Writer[B], g: B => Writer[C]): A => Writer[C] = {
    (x: A) => {
      val p1: Writer[B] = f(x)
      val p2: Writer[C] = g(p1.value)
      Writer(p2.value, s"${p1.log} ${p2.log}")
    }
  }

  def process(s: String): Writer[List[String]] = {
    compose(uppercase, words)(s)
  }

  def main(args: Array[String]) = {
    val composition = process("Hello World! Today is a great day for scala coding!")

    println(s"Result: ${composition.value}\nLog: ${composition.log}")
  }
}
/* Even though we ended up writting more code, it's completly worth it since we now have a much more reliable system than before (specially as it scales).
 * Furthermore, the compose and process method is usually already define for us so we won't be spending too much time writting this type of code.
 */
