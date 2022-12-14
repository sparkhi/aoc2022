package com.knowingwhere.aoc2022

import com.knowingwhere.aoc2022.util.Position
import org.scalatest.Matchers._
import org.scalatest._


class Day9Test extends WordSpec with BeforeAndAfterEach{
  "Movement of the rope " should {
    "correctly move the rope upwards only oqne step " in {
      val rope = List(Position(4,1), Position(3,0), Position(2,0), Position(1,0), Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0))
        val newRope = Day9.moveEachKnot(rope.tail, List(Position(rope.head.x, rope.head.y + 1)))
        newRope shouldBe List(Position(4,2), Position(4,1), Position(3,1), Position(2,1), Position(1,1),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0))
    }

    "correctly move the rope upwards 4 steps" in {
      val rope = List(Position(4,0), Position(3,0), Position(2,0), Position(1,0), Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0),Position(0,0))
      val newRope = Day9.moveUp(rope, 4, Set(Position(0, 0)))._1
      newRope shouldBe List(Position(4,4), Position(4,3), Position(4,2), Position(3,2), Position(2,2),Position(1,1),Position(0,0),Position(0,0),Position(0,0),Position(0,0))
    }
  }
}
