package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val message = string2Chars("hello")
    val messageCodeTree = Fork(Leaf('l',2),Fork(Fork(Leaf('h',1),Leaf('o',1),List('h', 'o'),2),Leaf('e',1),List('h', 'o', 'e'),3),List('h', 'o', 'e', 'l'),5)
    val messageSecret = List(1,0,0,1,1,0,0,1,0,1)
    val messageCodeTable = List(('l', List(0)), ('h', List(1,0,0)), ('o', List(1,0,1)), ('e', List(1,1)))
	}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times") {
    new TestTrees {
      assert(times(message).diff(List(('h', 1), ('e', 1), ('l', 2), ('o', 1))).isEmpty)
    }
  }

  test("tree is singleton") {
    new TestTrees {
      val list = List(t1)
      assert(singleton(list))
    }
  }

  test("tree is not singleton") {
    new TestTrees {
      val list = List(t1, t2)
      assert(!singleton(list))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list - send to tail") {
    val leafList = List(Leaf('a', 4), Fork(Leaf('b', 2), Leaf('c', 3), List('b', 'c'), 5), Leaf('d', 6))
    assert(combine(leafList) === List(Leaf('d', 6), Fork(Leaf('a', 4), Fork(Leaf('b', 2), Leaf('c', 3), List('b', 'c'), 5), List('a', 'b', 'c'), 9)))
  }

  test("combine of singleton leaf list") {
    new TestTrees {
      val leafList = List(t1)
      assert(combine(leafList) === leafList)
    }
  }

  test("until of some leaf list") {
    val leafList = List(Leaf('a', 1), Fork(Leaf('b', 1), Leaf('c', 2), List('b', 'c'), 3), Leaf('e', 4))
    assert(until(singleton, combine)(leafList) == List(Fork(Fork(Leaf('a', 1), Fork(Leaf('b', 1), Leaf('c', 2), List('b', 'c'), 3), List('a', 'b', 'c'), 4), Leaf('e', 4), List('a', 'b', 'c', 'e'), 8)))
  }

  test("until some singleton leaf list") {
    new TestTrees {
      val leafList = List(t2)
      assert(until(singleton, combine)(leafList) === leafList)
    }
  }

  test("decode message") {
    new TestTrees {
      assert(decode(messageCodeTree, messageSecret) == message)
    }
  }

  test("encode message with code tree") {
    new TestTrees {
      assert(encode(messageCodeTree)(message) === messageSecret)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("code bits of message with valid character") {
    new TestTrees {
      assert(codeBits(messageCodeTable)('h') === List(1,0,0))
    }
  }

  test("code bits of message with invalid character") {
    new TestTrees {
      intercept[IllegalArgumentException] {
        codeBits(messageCodeTable)('f')
      }
    }
  }

  test("convert code tree to table") {
    new TestTrees {
      assert(convert(messageCodeTree) === messageCodeTable)
    }
  }

  test("encode message with code table") {
    new TestTrees {
      assert(quickEncode(messageCodeTree)(message) == messageSecret)
    }
  }
}
