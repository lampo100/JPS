import org.scalatest._
import src.{AVL, Node}

class AVLSpec extends FlatSpec {

  "An empty AVLTree" should "add value to itself" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(3)
    assert( new_tree.find(3) )
  }

  it should "find them level deeper than root" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(1).add(2).add(3).add(5)
    assert( new_tree.find(5) )
  }

  it should "not add the same value twice" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(1).add(1).add(1).add(1)
    assert( new_tree.height() == 0 )
  }

  it should "add first value as it's root" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(2)
    val node_opt = new_tree.getRoot()
    node_opt match {
      case None => fail("Tree root should not be None")
      case Some(node) =>
        val Node(value, _, _) = node
        assert(value == 2)
    }
  }

  it should "add them in proper order" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(5).add(2).add(6)
    new_tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => assert(left.value == 2)
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) => assert(right.value == 6)
        }
    }
  }

  it should "add them in proper order if they are reversed" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(5).add(6).add(2)
    new_tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => assert(left.value == 2)
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) => assert(right.value == 6)
        }
    }
  }

  it should "perform left rotation" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(1).add(2).add(3)
    new_tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == 2)
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => assert(left.value == 1)
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) => assert(right.value == 3)
        }
    }
  }

  it should "perform right rotation" in {
      val tree = new AVL[Int]
      val new_tree = tree.add(3).add(2).add(1)
      new_tree.getRoot() match {
        case None => fail("Tree root should not be None")
        case Some(root) =>
          assert(root.value == 2)
          root.left match {
            case None => fail("left subtree should not be None")
            case Some(left) => assert(left.value == 1)
          }
          root.right match {
            case None => fail("right subtree should not be None")
            case Some(right) => assert(right.value == 3)
          }
      }

  }

  it should "throw if calling max" in {
    val tree = new AVL[Int]
    assertThrows[RuntimeException](tree.max())
  }

  it should "throw if calling min" in {
    val tree = new AVL[Int]
    assertThrows[RuntimeException](tree.min())
  }

  it should "result in an empty intersection" in {
    val tree = new AVL[Int]
    val tree_2 = new AVL[Int].add(1).add(2).add(3)
    val root = tree.intersection(tree_2).getRoot()
    root match {
      case Some(_) => fail("root should be empty")
      case None => assert(true)
    }
  }

  it should "contain all values from second tree after merge" in {
    val tree = new AVL[Int]
    val tree_2 = new AVL[Int].add(1).add(2).add(3)
    val final_tree = tree.merge(tree_2)
    assert(final_tree.find(1))
    assert(final_tree.find(2))
    assert(final_tree.find(3))
  }

  "Non empty AVLTree" should "remove a value from itself" in {
    val tree = new AVL[Int].add(5).add(7).add(3).add(1).add(9).add(10)
    val new_tree = tree.remove(3)
    assert(!new_tree.find(3))
  }

  it should "find a maximum value" in {
    val tree = new AVL[Int].add(5).add(7).add(3).add(1).add(9).add(10)
    assert( tree.max() == 10 )
  }

  it should "find minimum value" in {
    val tree = new AVL[Int].add(5).add(7).add(3).add(1).add(9).add(10)
    assert( tree.min() == 1 )
  }

  it should "contain values from both trees after merge" in {
    val tree = new AVL[Int].add(5).add(1)
    val tree2 = new AVL[Int].add(2).add(7)
    val final_tree = tree.merge(tree2)
    assert(final_tree.find(1))
    assert(final_tree.find(5))
    assert(final_tree.find(7))
    assert(final_tree.find(2))
  }

  it should "not double values in merge" in {
    val tree = new AVL[Int].add(5).add(7)
    val tree2 = new AVL[Int].add(2).add(7)
    val final_tree = tree.merge(tree2)
    assert(final_tree.height() == 1)
  }

  it should "be balanced after merge" in {
    val tree = new AVL[Int].add(5).add(7).add(89).add(2).add(234).add(67)
    val tree2 = new AVL[Int].add(2).add(1).add(3).add(4).add(8).add(9)
    val final_tree = tree.merge(tree2)
    assert(final_tree.balance() < 2 && final_tree.balance() > -2)
  }

  it should "contain only values present in both trees after intersection" in {
    val tree = new AVL[Int].add(2).add(1).add(5)
    val tree2 = new AVL[Int].add(2).add(1).add(3)
    val final_tree = tree.intersection(tree2)
    assert(final_tree.find(2))
    assert(final_tree.find(1))
    assert(!final_tree.find(5))
    assert(!final_tree.find(3))
    assert(final_tree.find(2))
    assert(final_tree.find(1))
    assert(!final_tree.find(5))
    assert(!final_tree.find(3))
  }

  it should "be empty if no common values are present" in {
    val tree = new AVL[Int].add(21).add(12).add(5)
    val tree2 = new AVL[Int].add(2).add(1).add(3)
    val root = tree.intersection(tree2).getRoot()
    root match {
      case Some(_) => fail("root should be empty")
      case None => assert(true)
    }
  }

  it should "be balanced after intersection" in {
    val tree = new AVL[Int].add(1).add(2).add(3).add(4).add(7)
    val tree2 = new AVL[Int].add(1).add(2).add(3).add(4).add(7)
    val final_tree = tree.intersection(tree2)
    assert(final_tree.balance() < 2 && final_tree.balance() > -2)
  }
}

