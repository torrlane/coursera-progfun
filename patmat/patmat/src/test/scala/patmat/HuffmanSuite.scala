package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("times a") {
    new TestTrees {
      assert(times( List('a' )  ) === List(('a', 1)) )
    }
  }

  test("times aaa") {
    new TestTrees {
      assert(times( List('a', 'a', 'a')  ) === List(('a', 3)) )
    }
  }

  test("times aba") {
    new TestTrees {
      assert(times( List('a', 'b', 'a')  ) === List(('a', 2), ('b', 1)) )
    }
  }

  test("times abab") {
    new TestTrees {
      assert(times( List('a', 'b', 'a', 'b')  ) === List(('b', 2), ('a', 2)) )
    }
  }

  test("times nothing") {
    new TestTrees {
      assert(times( List[Char]()  ) === List() )
    }
  }

  test("singleton with nothing") {
    new TestTrees {
      assert(singleton( List[CodeTree]()  ) === false )
    }
  }

  test("singleton with something") {
    new TestTrees {
      assert(singleton( List(Leaf('a', 1))  ) === true )
    }
  }

  test("singleton with more") {
    new TestTrees {
      assert(singleton( List(Leaf('a', 1), Leaf('b', 2))  ) === false )
    }
  }

  test("createCodetree 1") {
    new TestTrees {
      assert(createCodeTree(string2Chars("h")) === Leaf('h', 1))
    }
  }

  test("createCodetree 2") {
    new TestTrees {
      assert(createCodeTree(string2Chars("he")) === Fork( Leaf('h', 1), Leaf('e',1), List('h', 'e'), 2))
    }
  }

  test("createCodetree") {
    new TestTrees {
      val expected = Fork(Leaf('e',3),Fork(Leaf('h',1),Leaf('l',2),List('h', 'l'),3),List('e', 'h', 'l'),6)
      assert(createCodeTree(string2Chars("heeell")) === expected)
    }
  }
  

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode ") {
    new TestTrees {
      val tree = createCodeTree(string2Chars("heeell"))
      assert(decode(tree, List(0,1,1,1,0)) === "elh".toList)
    }
  }

  test("frenchCode ") {
    new TestTrees {
      val tree = createCodeTree(string2Chars("heeell"))
      assert(decodedSecret === "huffmanestcool".toList)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val tree = createCodeTree(string2Chars("heeello world"))
      assert(decode(tree, encode(tree)("hello world".toList)) === "hello world".toList)
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      val tree = createCodeTree(string2Chars("heeello world"))
      assert(decode(tree, quickEncode(tree)("heeello world".toList)) === "heeello world".toList)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
}
