package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times char list") {
    assert(times(string2Chars("aabccc")) == List(('a', 2), ('b', 1), ('c', 3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("create code tree") {
    val tree = createCodeTree(string2Chars("xett"))
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2))

    assert(tree === sampleTree)
  }

  test("decode") {
    val tree = createCodeTree(string2Chars("xett"))
    assert(decode(tree, List(1, 0, 0, 0, 1, 1)) === string2Chars("txet"))

  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("28 bit test") {
    val test = string2Chars("aaaaaaaabbbcdefgh")
    assert(encode(createCodeTree(test))(test.distinct).size == 28)
  }

  test("codebit test") {
    val codeTable: List[(Char, List[Bit])] = List(('c', List(1, 0, 1)))
    assert(codeBits(codeTable)('c') == List(1, 0, 1))
  }

  test("convert test") {
    val tree = createCodeTree(string2Chars("xett"))
    assert(convert(tree) == List(('x', List(0, 0)), ('e', List(0, 1)), ('t', List(1))))
  }

  test("quick encode") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
