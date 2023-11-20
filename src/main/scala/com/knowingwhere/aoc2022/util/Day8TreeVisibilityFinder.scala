package com.knowingwhere.aoc2022.util

import scala.annotation.tailrec

object Day8TreeVisibilityFinder {

  /**
   * return the indexes of those trees in the list whose height is more than any of the previous ones
   * e.g.
   * input heights of trees as 2,3,4,5 should return [0,1,2,3]
   * input heights of trees as 2,5,5,4,7 should return [0,1,4]
   * input heights of trees as 7,5,5,4,7 should return [0]
   *
   * @param treeline List with heights of trees
   * @return List of indexes within the treeline for the trees that are visible
   */
  def getVisibleTrees(treeline: List[Int]): List[Int] = {
    val indexes = findVisibleIndexesFromBeginning(treeline, 0, 0, List.empty[Int])
    indexes
  }

  @tailrec
  private def findVisibleIndexesFromBeginning(treeline: List[Int], maxHeight: Int, currentIndex: Int, indexes: List[Int]): List[Int] = {
    if (treeline.isEmpty) {
      indexes
    } else {
      val currentHeight = treeline.head
      if (currentHeight > maxHeight) {
        findVisibleIndexesFromBeginning(treeline.tail, currentHeight, currentIndex + 1, indexes :+ currentIndex)
      } else {
        findVisibleIndexesFromBeginning(treeline.tail, maxHeight, currentIndex + 1, indexes)
      }
    }
  }

  /**
   * return the forward viewing distances from each tree
   * e.g.
   * input heights of trees as 2,3,4,5 should return [1,1,1,0]
   * input heights of trees as 2,5,5,4,7 should return [1,1,2,1,0]
   * input heights of trees as 7,5,5,4,7 should return [4,1,2,1,0]
   *
   * @param treeline List with heights of trees
   * @return List of forward viewing distances from each tree
   */
  def getViewingDistanceAheadOfEachTree(treeline: List[Int]) : List[Int] = {
    val viewingDistances = findViewingDistances(treeline, List.empty[Int])
    viewingDistances
  }

  @tailrec
  private def findViewingDistances(treeline: List[Int], viewingDistances: List[Int]): List[Int] = {
    if (treeline.isEmpty) {
      viewingDistances
    } else {
      val currentHeight = treeline.head
      val viewingDistance = findDistanceToNextBlockingTree(treeline, currentHeight, 0)
      findViewingDistances(treeline.tail, viewingDistances :+ viewingDistance)
    }
  }

  @tailrec
  private def findDistanceToNextBlockingTree(treeline: List[Int], currentHeight: Int, distanceToTree: Int): Int = {
    if (treeline.tail.isEmpty) {
      distanceToTree
    } else {
      val newTreeLine = treeline.tail
      if (newTreeLine.head < currentHeight) {
        findDistanceToNextBlockingTree(newTreeLine, currentHeight, distanceToTree + 1)
      } else {
        distanceToTree + 1
      }
    }
  }

}
