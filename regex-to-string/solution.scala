import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

object Solution {

    def main(args: Array[String]) {

        val n: Int = readLine().toInt

        if (X.DEBUG) {
            println(n)
        }

        val re: String = readLine()

        val startNode: Node = Parser.interpretStr(re)

        val x = searchPattern(startNode, List(), n)

        if (x != null) {
            println(x.reverse.mkString(""))
        } else {
            println("NIL")
        }

    }

    def searchPattern(node: Node, pattern: List[Char], size: Int): List[Char] = {

        if (Solution.patternCache.contains(Tuple2(node.id, pattern.mkString("")))) {
            if (X.DEBUG) {
                println("cache hit")
                println(node, pattern.mkString(""))
            }
            return null
        }

        Solution.patternCache += Tuple2(node.id, pattern.mkString(""))
        
        var result = pattern

        if (X.DEBUG) {
            println(result.reverse)
            println(node)
        }

        if (node.char != '_') {
            result = node.char :: pattern
        }

        if (X.DEBUG) {
            println(result.reverse)
            println(result.length)
        }

        if (result.length == size && node.state == State.End) {
            return result

        } else if (result.length > size) {
            return null

        } else {
            if (node.nexts.length == 1) {
                return searchPattern(node.nexts.head, result, size)
            }

            for (node: Node <- node.nexts) {

                val searchResult = searchPattern(node, result, size)

                if (searchResult != null) {
                    return searchResult
                }
            }
        }

        null
    }

    var patternCache: Set[Tuple2[Int, String]] = Set[Tuple2[Int, String]]()

}

object X {
    val DEBUG = false
}


object Global {
    var counter = 0
}

object State extends Enumeration {
    val Start, Other, End = Value
}

class Node(state0: State.Value, char0: Char = '_') {

    Global.counter += 1

    var state = state0
    var char = char0

    val id = Global.counter

    val nexts = new ListBuffer[Node]()

    def appendNext(node: Node) = {
        this.nexts += node

        this
    }

    override def toString = "Node(id=" + id + ",char=" + char + ",state=" + state + ",nexts=" + nexts.map(_.id) + ")"

}

object Parser {

    def parse(chrs: List[Char]): (List[Any], List[Char]) = {

        var chars = chrs

        var seq = new Stack[Any]()
        val alt = new Stack[Any]()

        while (!chars.isEmpty) {

            if (X.DEBUG) {
                println(chars)
                println(seq)
            }

            chars match {
                case '(' :: tail => {
                    val (parsed, rest) = parse(tail)
                    seq.push(parsed)
                    chars = rest
                }
                case ')' :: tail => {

                    if (alt.length > 0) {
                        alt.push(seq.toSeq.toList.reverse)

                        val result = List('|', alt.toSeq.toList.reverse)

                        return (result, chars.tail)
                    }

                    return (seq.toSeq.toList.reverse, chars.tail)
                }
                case '*' :: tail => {
                    val last = seq.pop()
                    seq.push('*', last)
                    chars = chars.tail
                }
                case '|' :: tail => {
                    alt.push(seq.toSeq.toList.reverse)
                    seq = new Stack[Any]()
                    chars = chars.tail
                }
                case head :: tail => {
                    seq.push(head)
                    chars = chars.tail
                }
                case nil => nil
            }
        }

        if (alt.length > 0) {
            alt.push(seq.toSeq.toList.reverse)

            val result = List('|', alt.toSeq.toList.reverse)

            return (result, chars)
        }

        (seq.toSeq.toList.reverse, chars)
    }

    def interpret(seq0: List[Any], node0: Node): Node = {

        var seq = seq0
        var node = node0

        while (!seq.isEmpty) {

            if (X.DEBUG) {
                println(seq)
                println(node)
            }

            seq match {

                case '|' :: tail => {
                    val endNode = new Node(State.Other)

                    if (X.DEBUG) {
                        println('|')
                        println(endNode.id)
                    }

                    val alts = tail.head
                    seq = tail.tail

                    alts match {
                        case alts0: List[Any] => {
                            for (altSeq <- alts0) {
                                altSeq match {
                                    case altSeq: List[Any] => {
                                        val altNode = new Node(State.Other)

                                        if (X.DEBUG) {
                                            println("altSeq")
                                            println(altNode.id)
                                        }

                                        node.appendNext(altNode)
                                        interpret(altSeq, altNode).appendNext(endNode)
                                    }
                                    case nil => nil
                                }
                            }
                        }
                        case nil => nil
                    }

                    node = endNode
                }
                case '*' :: tail => {
                    val t = tail.head
                    seq = tail.tail

                    if (X.DEBUG) {
                        println('*')
                        println(seq)
                    }

                    val inNode = new Node(State.Other)
                    val outNode = new Node(State.Other)

                    t match {
                        case c: Char => {
                            Parser.interpret(List(c), inNode).appendNext(outNode).appendNext(inNode)
                        }
                        case x: List[Any] => {
                            Parser.interpret(x, inNode).appendNext(outNode).appendNext(inNode)
                        }
                        case nil => nil
                    }

                    node.appendNext(outNode)
                    node.appendNext(inNode)

                    if (X.DEBUG) {
                        println("*'s previous node is:")
                        println(node)
                        println("*'s out node is:")
                        println(outNode)
                        println("*'s in node is:")
                        println(inNode)
                    }

                    node = outNode
                }
                case head :: tail => {

                    head match {
                        case c: Char => {
                            val charNode = new Node(State.Other, c)

                            node.appendNext(charNode)
                            node = charNode
                        }
                        case c: List[Any] => {
                            node = Parser.interpret(c, node)
                        }
                        case nil => {
                        }
                    }

                    seq = tail
                }
                case nil => nil
            }
        }

        if (X.DEBUG) {
            println("endNode.id is:")
            println(node.id)
        }

        node
    }

    def interpretStr(str: String): Node = {

        val startNode = new Node(State.Start)
        val (parsed, x) = Parser.parse(str.toList)

        val endNode = Parser.interpret(parsed, startNode)

        endNode.state = State.End

        startNode
    }

}


/*

def testNode() = {
    val node = new Node(State.Start, 'c')
    println(node)
    println(node.nexts)
    println(node.id)
}

def testParse() {
    println(Parser.parse("a(b|c)*".toList))
}

def testInterpret() {
    val node = new Node(State.Start)
    val (parsed, x) = Parser.parse("abcd(efg|hij)*klmn".toList)
    val n = Parser.interpret(parsed, node)
    println(n)
}

def testInterpretStr() {
    val x = Parser.interpretStr("abcd(efg|hif)*klmn")

    println(x)
}
*/

//testInterpretStr()

//Solution.main(args)
