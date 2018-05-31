import org.scalatest._
import src.{AVL, Node}

import scala.util.Random

class AVLSpec extends FlatSpec {

  "An empty AVLTree" should "add value to itself" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(3)
    assert(new_tree.find(3))
  }

  it should "find them level deeper than root" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(1).add(2).add(3).add(5)
    assert(new_tree.find(5))
  }

  it should "not add the same value twice" in {
    val tree = new AVL[Int]
    val new_tree = tree.add(1).add(1).add(1).add(1)
    assert(new_tree.height() == 0)
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

  "The tree" should "do left rotation after removing 0" in {
    val tree = new AVL[Int].add(1).add(2).add(0).add(3)
    tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == 1)
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => assert(left.value == 0)
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) => {
            assert(right.value === 2)
            right.right match {
              case None => fail("right-right subtree should not be None")
              case Some(rr) => assert(rr.value == 3)
            }
          }
        }
    }
    val new_tree = tree.remove(0)
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

  it should "do right rotation after removing 0" in {
    val tree = new AVL[Int].add(1).add(2).add(0).add(-1)
    tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == 1)
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => {
            assert(left.value == 0)
            left.left match {
              case None => fail("left-left subtree should not be None")
              case Some(ll) => assert(ll.value == -1)
            }
          }
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) => assert(right.value === 2)
        }
    }
    val new_tree = tree.remove(2)
    new_tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == 0)
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => assert(left.value == -1)
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) => assert(right.value == 1)
        }
    }
  }

  it should "do left-right rotation after removing" in {
    val firstNode = Node[Int](-1, None, None)
    val secondNode = Node[Int](-2, None, Some(firstNode))
    val rootNode = Node[Int](0, Some(secondNode), Some(Node[Int](22, None, None)))

    val tree = new AVL[Int](Some(rootNode))
    tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == 0)
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) =>{
            assert(left.value == -2)
            left.right match {
              case None => fail("left-right subtree should not be None")
              case Some(right) => assert(right.value == -1)
            }
          }
        }
    }
    val new_tree = tree.remove(22)
    new_tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == -1)
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => assert(left.value == -2)
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(l_right) => assert(l_right.value == 0)
        }
    }
  }

  it should "do right-leftt rotation after removing" in {
    val firstNode = Node[Int](1, None, None)
    val secondNode = Node[Int](2, Some(firstNode), None)
    val rootNode = Node[Int](0, Some(Node[Int](-12, None, None)), Some(secondNode))

    val tree = new AVL[Int](Some(rootNode))
    tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == 0)
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) =>{
            assert(right.value == 2)
            right.left match {
              case None => fail("right-left subtree should not be None")
              case Some(r_left) => assert(r_left.value == 1)
            }
          }
        }
    }
    val new_tree = tree.remove(-12)
    new_tree.getRoot() match {
      case None => fail("Tree root should not be None")
      case Some(root) =>
        assert(root.value == 1)
        root.left match {
          case None => fail("left subtree should not be None")
          case Some(left) => assert(left.value == 0)
        }
        root.right match {
          case None => fail("right subtree should not be None")
          case Some(right) => assert(right.value == 2)
        }
    }
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

  def fixture =
    new {
      val firstNode = Node[Int](253, None, None)
      val secondNode = Node[Int](-22, None, None)
      val thirdNode = Node[Int](100, Some(secondNode), Some(firstNode))
      val fourthNode = Node[Int](16000, None, None)
      val rootNode = Node[Int](3133, Some(thirdNode), Some(fourthNode))
      val first_tree = new AVL[Int](Some(rootNode))
    }

  "Find" should "return true when searching for all values in the tree" in {
    assert(fixture.first_tree.find(253) === true)
    assert(fixture.first_tree.find(-22) === true)
    assert(fixture.first_tree.find(100) === true)
    assert(fixture.first_tree.find(16000) === true)
    assert(fixture.first_tree.find(3133) === true)
  }
  it should "return false when searching for missing values" in {
    assert(fixture.first_tree.find(11111) === false)
    assert(fixture.first_tree.find(-1231231) === false)
    assert(fixture.first_tree.find(0) === false)
  }

  it should " return false for empty tree for any value" in {
    val empty_tree = new AVL[Int]
    for (x <- -1000000 to 1000000) {
      assert(empty_tree.find(x) === false)
    }
  }

  "Balance" should "return 0 for empty tree" in {
    val empty_tree = new AVL[Int]
    assert(empty_tree.balance() === 0)
  }

  it should "return 0 for balanced tree" in {
    var tree = new AVL[Int].add(0).add(2).add(-2)
    assert(tree.balance() === 0)
    tree = tree.add(-4).add(5)
    assert(tree.balance() === 0)
  }

  it should "return 1 for a tree that has one more node on the right side" in {
    var tree = new AVL[Int].add(0).add(2).add(-2).add(54)
    assert(tree.balance() === 1)
    tree = tree.add( 55)
    assert(tree.balance() === 1)
  }

  it should "return only -1 for a tree that has one more node on the left side" in {
    var tree = new AVL[Int].add(0).add(2).add(-2).add(-222)
    assert(tree.balance() === -1)
    tree = tree.add( -55)
    assert(tree.balance() === -1)
  }

  "The tree" should "balance itself meaning that no matter how many nodes we add, the balance should always be either -1,0 or 1" in {
    var tree = new AVL[Int]
    val rand = new Random()
    for(x <- 0 to 2000){
      tree = tree.add(rand.nextInt(200000000))
      val balance = tree.balance()
      assert(balance === -1 || balance === 0 || balance === 1)
    }
  }

  "The height of empty tree" should "be -1" in {
    var tree = new AVL[Int]
    assert(tree.height() === -1)
  }

  "The height of the tree containing one node" should "be 0" in {
    val tree = new AVL[Int].add(10)
    assert(tree.height() === 0)
  }

  "The height of the tree containing two nodes" should "be 1" in {
    var tree = new AVL[Int]
    tree = tree.add(10).add(11)
    assert(tree.height() === 1)
  }

  "The height of the balanced binary tree containing values from 0 to 1024" should "be 10 (log2(1024))" in {
    var tree = new AVL[Int]
    for(x <- 0 to 1024){
      tree = tree.add(x)
    }
    assert(tree.height() === 10)
  }
}

