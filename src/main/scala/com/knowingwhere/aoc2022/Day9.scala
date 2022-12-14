package com.knowingwhere.aoc2022

import com.knowingwhere.aoc2022.util.Position

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day9 extends App {
  val instructions = Source.fromResource("day9-input.txt").getLines().toList

  val initialHeadPosition = Position(0,0)
  val initialTailPosition = Position(0,0)
  val positionAccumulator = List(Position(0,0))

  val tailVisits = moveRope(instructions, initialHeadPosition, initialTailPosition, positionAccumulator)

  tailVisits.toSet.size.pipe(println)

  @tailrec
  def moveRope(instructions: List[String], headPosition: Position, tailPosition: Position, positionAccumulator: List[Position]):List[Position] = {

    if (instructions.isEmpty) {
      positionAccumulator
    } else {

      val currentInstruction = instructions.head.split(" ")
      val direction = currentInstruction(0)
      val steps = currentInstruction(1).toInt

      val something = direction match {
        case "R" =>
          moveRight(headPosition, tailPosition, steps, positionAccumulator)
        case "L" =>
          moveLeft(headPosition, tailPosition, steps, positionAccumulator)
        case "U" =>
          moveUp(headPosition, tailPosition, steps, positionAccumulator)
        case "D" =>
          moveDown(headPosition, tailPosition, steps, positionAccumulator)
      }
      moveRope(instructions.tail, something._1._1, something._1._2, something._2)
    }
  }

  @tailrec
  def moveRight(headPosition: Position, tailPosition: Position, stepsRemaining: Int, positionAccumulator: List[Position]): ((Position, Position), List[Position])  = {
    if (stepsRemaining == 0) {
      headPosition -> tailPosition -> positionAccumulator
    } else {
      val newHeadPosition = Position(headPosition.x + 1, headPosition.y)
      if (!isTouching(newHeadPosition, tailPosition)) {
        val newTailPosition = Position(newHeadPosition.x - 1, newHeadPosition.y)
        moveRight(newHeadPosition, newTailPosition, stepsRemaining - 1, positionAccumulator :+ newTailPosition)
      } else {
        moveRight(newHeadPosition, tailPosition, stepsRemaining - 1, positionAccumulator)
      }
    }
  }

  @tailrec
  def moveLeft(headPosition: Position, tailPosition: Position, stepsRemaining: Int, positionAccumulator: List[Position]): ((Position, Position), List[Position])  = {
    if (stepsRemaining == 0) {
      headPosition -> tailPosition -> positionAccumulator
    } else {
      val newHeadPosition = Position(headPosition.x - 1, headPosition.y)
      if (!isTouching(newHeadPosition, tailPosition)) {
        val newTailPosition = Position(newHeadPosition.x + 1, newHeadPosition.y)
        moveLeft(newHeadPosition, newTailPosition, stepsRemaining - 1, positionAccumulator :+ newTailPosition)
      } else {
        moveLeft(newHeadPosition, tailPosition, stepsRemaining - 1, positionAccumulator)
      }
    }
  }

  @tailrec
  def moveUp(headPosition: Position, tailPosition: Position, stepsRemaining: Int, positionAccumulator: List[Position]): ((Position, Position), List[Position])  = {
    if (stepsRemaining == 0) {
      headPosition -> tailPosition -> positionAccumulator
    } else {
      val newHeadPosition = Position(headPosition.x, headPosition.y + 1)
      if (!isTouching(newHeadPosition, tailPosition)) {
        val newTailPosition = Position(newHeadPosition.x, newHeadPosition.y - 1)
        moveUp(newHeadPosition, newTailPosition, stepsRemaining - 1, positionAccumulator :+ newTailPosition)
      } else {
        moveUp(newHeadPosition, tailPosition, stepsRemaining - 1, positionAccumulator)
      }
    }
  }

  @tailrec
  def moveDown(headPosition: Position, tailPosition: Position, stepsRemaining: Int, positionAccumulator: List[Position]): ((Position, Position), List[Position])  = {
    if (stepsRemaining == 0) {
      headPosition -> tailPosition -> positionAccumulator
    } else {
      val newHeadPosition = Position(headPosition.x, headPosition.y - 1)
      if (!isTouching(newHeadPosition, tailPosition)) {
        val newTailPosition = Position(newHeadPosition.x, newHeadPosition.y + 1)
        moveDown(newHeadPosition, newTailPosition, stepsRemaining - 1, positionAccumulator :+ newTailPosition)
      } else {
        moveDown(newHeadPosition, tailPosition, stepsRemaining - 1, positionAccumulator)
      }
    }
  }

  def isTouching(newHeadPosition: Position, tailPosition: Position): Boolean = {
    Math.abs(newHeadPosition.x - tailPosition.x) <= 1 && Math.abs(newHeadPosition.y - tailPosition.y) <= 1
  }

}
