package fpinscala.datastructures

import org.scalatest.{FunSpec, MustMatchers}

class TreeTest extends FunSpec with MustMatchers {

  val tree:Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

  it("size returns number of nodes in the tree") {
    Tree.size(tree) mustBe 5
  }

  it("maximum returns maximum value of a leaf in the tree") {
    Tree.maximum(tree) mustBe 3
  }

  it("depth returns the maximum path length from the root of a tree to any leaf") {
    Tree.depth(tree) mustBe 2
  }

  it("map modifies each element in a tree with a given function") {
    Tree.map(tree)(_.toString) mustBe Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
  }

}
