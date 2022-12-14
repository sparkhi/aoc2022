package com.knowingwhere.aoc2022

import com.knowingwhere.aoc2022.util.Position

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Day9 extends App {
  val instructions = Source.fromResource("day9-input.txt").getLines().toList

  val initialHeadPosition = Position(0,0)
  val initialTailPosition = Position(0,0)
  val positionAccumulator = Set(Position(0,0))

  val part1rope = List(Position(0,0), Position(0,0))
  moveRope(instructions, part1rope, positionAccumulator).size.pipe(println)

  val part2Rope = List(Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0))
  moveRope(instructions, part2Rope, positionAccumulator).size.pipe(println)


  /**
   * Starting of the simulation to move the rope through successive instructions (problem input)
   * @param instructions instructions about movement of the rope (e.g. U 10, R 12 etc)
   * @param rope Starting position of the rope
   * @param positionAccumulator the accumulator to keep the tail position (need of the problem)
   * @return the list of positions where the tail has visited
   */
  @tailrec
  def moveRope(instructions: List[String], rope: List[Position], positionAccumulator: Set[Position]):Set[Position] = {

    if (instructions.isEmpty) {
      positionAccumulator
    } else {

      val currentInstruction = instructions.head.split(" ")
      val direction = currentInstruction(0)
      val steps = currentInstruction(1).toInt

      val movedRopeAndTailPositions = direction match {
        case "R" =>
          moveRight(rope, steps, positionAccumulator)
        case "L" =>
          moveLeft(rope, steps, positionAccumulator)
        case "U" =>
          moveUp(rope, steps, positionAccumulator)
        case "D" =>
          moveDown(rope, steps, positionAccumulator)
      }
      moveRope(instructions.tail, movedRopeAndTailPositions._1, movedRopeAndTailPositions._2)
    }
  }

  /**
   * Move the rope right through successive movements of the knots over number of steps passed in
   * @param rope List of positions denoting starting location of each knot
   * @param stepsRemaining number of steps for iteration
   * @param positionAccumulator accumulates the last position after each movement (need of the problem)
   * @return Tuple2 containing the new position of rope and the tail position accumulation for next step
   */
  @tailrec
  def moveRight(rope: List[Position], stepsRemaining: Int, positionAccumulator: Set[Position]): (List[Position], Set[Position])  = {
    if (stepsRemaining == 0) {
      rope -> positionAccumulator
    } else {
      val newRope = moveEachKnot(rope.tail, List(Position(rope.head.x + 1, rope.head.y)))
      moveRight(newRope, stepsRemaining - 1, positionAccumulator + newRope.reverse.head)
    }
  }

  /**
   * Move the rope left through successive movements of the knots over number of steps passed in
   * @param rope List of positions denoting starting location of each knot
   * @param stepsRemaining number of steps for iteration
   * @param positionAccumulator accumulates the last position after each movement (need of the problem)
   * @return Tuple2 containing the new position of rope and the tail position accumulation for next step
   */
  @tailrec
  def moveLeft(rope: List[Position], stepsRemaining: Int, positionAccumulator: Set[Position]): (List[Position], Set[Position])  = {
    if (stepsRemaining == 0) {
      rope -> positionAccumulator
    } else {
      val newRope = moveEachKnot(rope.tail, List(Position(rope.head.x -1, rope.head.y)))
      moveLeft(newRope, stepsRemaining - 1, positionAccumulator + newRope.reverse.head)
    }
  }

  /**
   * Embeds the logic of how each knot moves with respect to the movement of the previous knot
   * @param head position of the MOVED head
   * @param tail position of the current knot
   * @return position where the current knot moves to
   */
  def getNewKnotPosition(head: Position, tail: Position): Position = {
    if (isTouching(head, tail)) {
      tail
    } else {
      if (head.x == tail.x) {
        Position(head.x, (head.y + tail.y) / 2) //simple movement in X direction
      } else if (head.y == tail.y) {
        Position((head.x + tail.x) / 2, head.y) //simple movement in y direction
      } else {
        if ((Math.abs(head.x - tail.x) > 1) && Math.abs(head.y - tail.y) > 1) { //diagonal movement to keep up
          Position((head.x + tail.x) / 2, (head.y + tail.y) / 2)
        }
        else if (Math.abs(head.x - tail.x) > 1) { //diagonal movement to match the row
          Position((head.x + tail.x) / 2, head.y)
        } else if (Math.abs(head.y - tail.y) > 1) { //diagonal movement to match the column
          Position(head.x, (head.y + tail.y) / 2)
        } else {
          throw new IllegalArgumentException("Unexpected pair of positions " + head + " and " + tail)
        }
      }
    }
  }

  /**
   * tail recursive function to calculate the new position of the rope through one movement
   * @param remainingPieceOfRope the list of knots to be moved
   * @param newRopeAccumulator accumulate the new positions, caller starts with the head knot moved
   * @return List of positions containing new positions of all knots
   */
  @tailrec
  def moveEachKnot(remainingPieceOfRope: List[Position], newRopeAccumulator: List[Position]): List[Position] = {
    if (remainingPieceOfRope.isEmpty) {
      newRopeAccumulator
    } else {
      val previousKnot = newRopeAccumulator.reverse.head
      val newCurrentKnot = getNewKnotPosition(previousKnot, remainingPieceOfRope.head)
      moveEachKnot(remainingPieceOfRope.tail, newRopeAccumulator :+ newCurrentKnot)
    }
  }


  /**
   * Move the rope upwards through successive movements of the knots over number of steps passed in
   * @param rope List of positions denoting starting location of each knot
   * @param stepsRemaining number of steps for iteration
   * @param positionAccumulator accumulates the last position after each movement (need of the problem)
   * @return Tuple2 containing the new position of rope and the tail position accumulation for next step
   */
  @tailrec
  def moveUp(rope: List[Position], stepsRemaining: Int, positionAccumulator: Set[Position]): (List[Position], Set[Position])  = {
    if (stepsRemaining == 0) {
      rope -> positionAccumulator
    } else {
      val newRope = moveEachKnot(rope.tail, List(Position(rope.head.x, rope.head.y + 1)))
      moveUp(newRope, stepsRemaining - 1, positionAccumulator + newRope.reverse.head)
    }
  }

  /**
   * Move the rope downwards through successive movements of the knots over number of steps passed in
   * @param rope List of positions denoting starting location of each knot
   * @param stepsRemaining number of steps for iteration
   * @param positionAccumulator accumulates the last position after each movement (need of the problem)
   * @return Tuple2 containing the new position of rope and the tail position accumulation for next step
   */
  @tailrec
  def moveDown(rope: List[Position], stepsRemaining: Int, positionAccumulator: Set[Position]): (List[Position], Set[Position])  = {
    if (stepsRemaining == 0) {
      rope -> positionAccumulator
    } else {
      val newRope = moveEachKnot(rope.tail, List(Position(rope.head.x, rope.head.y - 1)))
      moveDown(newRope, stepsRemaining - 1, positionAccumulator + newRope.reverse.head)
    }
  }

  /**
   * Are the 2 positions touching each other in either horizontal, vertical or diagonal direction
   * @param positionOne First position for comparison
   * @param positionTwo Second position for comparison
   * @return Boolean indicating whether the 2 positions touch each other
   */
  def isTouching(positionOne: Position, positionTwo: Position): Boolean = {
    Math.abs(positionOne.x - positionTwo.x) <= 1 && Math.abs(positionOne.y - positionTwo.y) <= 1
  }
}
