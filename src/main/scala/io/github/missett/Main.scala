package io.github.missett

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

case class Balloon(max: Int, inflations: Int)

sealed trait UserAction
case object Inflate extends UserAction
case object Bank extends UserAction

case class State(balloons: List[Balloon], score: Int)


object Main extends App {
  type StateTransitionLog = List[String]

  // we could easily just use printOutput inline with the state transition logic but
  // this allows us to keep it nice and pure, and we can also test the logging
  def log(line: String, logs: StateTransitionLog = List.empty): List[String] =
    line :: logs

  def transition(state: State, action: UserAction): (State, StateTransitionLog) = state.balloons match {
    case balloon :: tail => action match {
      case Inflate =>
        val updated = balloon.copy(inflations = balloon.inflations + 1)

        if (updated.inflations > updated.max) {
          (State(tail, state.score), log("BURST"))
        } else {
          (State(updated :: tail, state.score), log("CONTINUE"))
        }
      case Bank =>
        (State(tail, state.score + balloon.inflations), log("BANKED"))
    }
    case _ => (state, log("FINISHED"))
  }

  // if any token is bad then the entire input fails, normally would use something like cats .sequence op or maybe Validated
  // TODO allow only non-negative ints
  def parseBalloonConfig(input: String): Either[Throwable, List[Int]] = (input.split(" ").toList.map(token => Try(token.toInt)).partition(_.isSuccess) match {
    case (result, Nil) => Success(result.map(_.get))
    case (_, errors) => Failure(new RuntimeException(s"could not understand your balloons, got the following failures - ${errors.mkString(", ")}"))
  }).toEither

  def parseUserAction(input: String): Either[Throwable, UserAction] = input.trim.toLowerCase match {
    case "inflate" => Right(Inflate)
    case "bank" => Right(Bank)
    case _ => Left(new RuntimeException(s"could not understand your choice of action, please enter one of [inflate, bank]"))
  }

  def getInput: Either[Throwable, String] = Right(StdIn.readLine(">>> "))

  def printOutput(output: String): Either[Throwable, Unit] = Right(println(s"<<< $output"))

  def runInflationLoop(state: State): Either[Throwable, State] = for {
    actionInput       <- getInput
    action            <- parseUserAction(actionInput)
    transitionOutcome <- Right(transition(state, action))
    updatedState      <- Right(transitionOutcome._1)
    updatedLogs       <- Right(transitionOutcome._2)
    _                 <- printOutput(updatedLogs.mkString("\n"))
    thing             <- updatedState.balloons match {
      case _ :: _ => runInflationLoop(updatedState)
      case _      => Right(updatedState)
    }
  } yield thing

  def run(): Either[Throwable, Int] = for {
    balloonsInput  <- getInput
    balloonsConfig <- parseBalloonConfig(balloonsInput)
    balloons       <- Right(balloonsConfig.map(Balloon(_, 0)))
    result         <- runInflationLoop(State(balloons, 0))
    score          <- Right(result.score)
  } yield score

  run() match {
    case Right(score) => printOutput(s"your final score was $score")
    case Left(error) => printOutput(s"oops, there was an error - ${error.getMessage}")
  }
}