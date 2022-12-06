package com.knowingwhere.aoc2022

import org.scalatest.Matchers._
import org.scalatest._

class Day6Test extends WordSpec with BeforeAndAfterEach{
  "The unique finder" should {
    "find position of unique set characters" in {
      Day6.findUnique("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 5
      Day6.findUnique("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 6
      Day6.findUnique("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 10
      Day6.findUnique("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 11
    }
  }
}
