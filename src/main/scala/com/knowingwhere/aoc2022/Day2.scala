package com.knowingwhere.aoc2022

import scala.io.Source

object Day2 extends App {
  val strategies = Source.fromResource("day2-input.txt").getLines().toList
  val scores = strategies.map(eachRound => calculateScorePart1(eachRound))
  println(scores.sum)
  val scores2 = strategies.map(eachRound => calculateScorePart2(eachRound))
  println(scores2.sum)


  private def calculateChoiceScore(choice: String) = {
    choice match {
      case "X" => 1
      case "Y" => 2
      case "Z" => 3
      case _ => throw new IllegalArgumentException
    }
  }

  private def calculateWinningScore(choiceOne: String, choiceTwo: String) = {
    choiceOne match {
      case "A" => //Rock
        choiceTwo match {
          case "X" => 3 //Rock
          case "Y" => 6 //Paper
          case "Z" => 0 //Scissors
        }
      case "B" => //Paper
        choiceTwo match {
          case "X" => 0 //Rock
          case "Y" => 3 //Paper
          case "Z" => 6 //Scissors
        }
      case "C" => //Scissors
        choiceTwo match {
          case "X" => 6 //Rock
          case "Y" => 0 //Paper
          case "Z" => 3 //Scissors
        }
    }
  }

  /**
   * first letter is either of (A, B, C) for Rock Paper Scissors respectively
   * second letter is either of (X, Y, Z) for Rock Paper Scissors respectively
   * Score calculation is done from the POV of column 2 and the rules are:
   * Round score: Win - earns 6 points, Draw - earns 3 points, Loss - earns 0 points
   * Choice score: Rock - earns 1 point, Paper - earns 2 points, Scissors - earns 3 points
   *
   * @param eachRound String of the form "A X"
   * @return score for the round of RPS
   */
  private def calculateScorePart1(eachRound: String): Int = {
    val choices = eachRound.split(" ").toList
    calculateChoiceScore(choices.tail.head) + calculateWinningScore(choices.head, choices.tail.head)
  }

  /**
   * New instructions:
   * "Anyway, the second column says how the round needs to end:
   * X means you need to lose,
   * Y means you need to end the round in a draw,
   * and Z means you need to win.
   *
   * transpose the second parameter before calculating the score in exactly same way
   *
   * @param eachRound String of the form "A X"
   * @return score for the round of RPS
   */
  private def calculateScorePart2(eachRound: String): Int = {
    val choices = eachRound.split(" ").toList
    val revisedChoiceTwo = translateInstruction(choices.head, choices.tail.head)
    calculateChoiceScore(revisedChoiceTwo) + calculateWinningScore(choices.head, revisedChoiceTwo)
  }

  def translateInstruction(choice1: String, isWinLoseOrDraw: String) = {
    isWinLoseOrDraw match {
      case "X" => //need to lose
        choice1 match {
          case "A" => "Z" //Rock
          case "B" => "X" //Paper
          case "C" => "Y" //Scissors
        }
      case "Y" => //Draw
        choice1 match {
          case "A" => "X" //Rock
          case "B" => "Y" //Paper
          case "C" => "Z" //Scissors
        }
      case "Z" => //Win
        choice1 match {
          case "A" => "Y" //Rock
          case "B" => "Z" //Paper
          case "C" => "X" //Scissors
        }
    }
  }
}
