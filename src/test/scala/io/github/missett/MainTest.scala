package io.github.missett

import org.scalatest.{EitherValues, FlatSpec, Matchers, TryValues}

class MainTest extends FlatSpec with Matchers with TryValues with EitherValues {
  behavior of "transition"

  it should "return the same state when there are no more balloons and the user Inflates" in {
    val init = State(List.empty[Balloon], 0)
    Main.transition(init, Inflate) should equal (init)
  }

  it should "return the same state when there are no more balloons and the user Banks" in {
    val init = State(List.empty[Balloon], 0)
    Main.transition(init, Bank) should equal (init)
  }

  it should "increase the score by the number of inflations and then progress to the next balloon when the user Banks" in {
    val first = Balloon(1, 1)
    val second = Balloon(1, 0)
    val balloons = List(first, second)
    val state = State(balloons, 0)
    val output = Main.transition(state, Bank)

    output should equal (State(List(second), first.inflations))
  }

  it should "progress to the next balloon without increasing the score when the user Inflates and the balloon bursts" in {
    val first = Balloon(1, 1)
    val second = Balloon(1, 0)
    val balloons = List(first, second)
    val state = State(balloons, 0)
    val output = Main.transition(state, Inflate)

    output should equal (State(List(second), state.score))
  }

  behavior of "parseBalloonsConfig"

  it should "return a list of ints when the input is valid" in {
    Main.parseBalloonConfig("1 2 3 4").right.value should equal (List(1, 2, 3, 4))
  }

  it should "return a failure if any of the tokens is not an int" in {
    val failure = "bad input detected, got the following failures - Failure(java.lang.NumberFormatException: For input string: \"foo\")"
    Main.parseBalloonConfig("1 foo 3 4").left.value.getMessage should equal (failure)
  }

  it should "coalesce multiple failures into one failure" in {
    val failure = "bad input detected, got the following failures - Failure(java.lang.NumberFormatException: For input string: \"foo\"), Failure(java.lang.NumberFormatException: For input string: \"bar\")"
    Main.parseBalloonConfig("1 foo 3 bar").left.value.getMessage should equal (failure)
  }

  behavior of "parseUserAction"

  it should "return a Bank when user enters bank" in {
    Main.parseUserAction("bank").right.value should equal (Bank)
  }

  it should "return Inflate when user enter inflate" in {
    Main.parseUserAction("inflate").right.value should equal (Inflate)
  }

  it should "return Bank and Inflate when the user enters the correct word with different capitalization or surrounded by whitespace" in {
    Main.parseUserAction("Inflate").right.value should equal (Inflate)
    Main.parseUserAction("Bank").right.value should equal (Bank)
    Main.parseUserAction("Inflate  ").right.value should equal (Inflate)
    Main.parseUserAction(" Bank").right.value should equal (Bank)
  }

  it should "return a Left with an error when the user enters a value that cannot be parsed" in {
    Main.parseUserAction("foo").left.value.getMessage should equal ("bad input detected, please enter one of [inflate, bank]")
  }

  behavior of "act"

  it should "do something" in {

  }
}