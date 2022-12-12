package com.knowingwhere.aoc2022.util

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, WordSpec}

class Day8TreeVisibilityFinderTest extends WordSpec with BeforeAndAfterEach{
  //  * input heights of trees as 2,3,4,5 should return [0,1,2,3]
  //  * input heights of trees as 2,5,5,4,7 should return [0,1,4]
  //  * input heights of trees as 7,5,5,4,7 should return [0]

  "Tree visibility finder" should {
    "find the indexes of trees when they are all in order " in {
      val visibleIndexes = Day8TreeVisibilityFinder.getVisibleTrees(List(2, 3, 4, 5))
      visibleIndexes.size shouldBe 4
      visibleIndexes shouldEqual List(0,1,2,3)
    }
    "find the indexes of trees when they are not in order " in {
      val visibleIndexes = Day8TreeVisibilityFinder.getVisibleTrees(List(2, 5, 5, 4, 7))
      visibleIndexes.size shouldBe 3
      visibleIndexes shouldEqual List(0,1,4)
    }
    "find the indexes of trees when first tree is blocking all other " in {
      val visibleIndexes = Day8TreeVisibilityFinder.getVisibleTrees(List(7, 5, 5, 4, 7))
      visibleIndexes.size shouldBe 1
      visibleIndexes shouldEqual List(0)
    }
  }
}
