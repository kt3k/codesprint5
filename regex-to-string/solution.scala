import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

object Solution {

    def main(args: Array[String]) {

        val n: Int = readLine().toInt

        println(n)

        val re: String = readLine()

        re.split("").foreach(println(_))

    }

    def searchPattern(node: Node, pattern: String, size: Int): String = {
        null
    }
}

val DEBUG = true

object Global {
    var counter = 0
}

object State extends Enumeration {
    val Start, Other, End = Value
}

class Node(state: State.Value, char: Char = '_') {

    Global.counter += 1

    val id = Global.counter

    val nexts = new ListBuffer[Node]()

    def appendNext(node: Node) = {
        this.nexts += node

        this
    }

    override def toString = "Node(id=" + id + ",nexts=" + nexts + ")"

}

object Parser {

    def parse(chrs: List[Char]): (List[Any], List[Char]) = {

        var chars = chrs

        var seq = new Stack[Any]()
        val alt = new Stack[Any]()

        while (!chars.isEmpty) {

            if (DEBUG) {
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

        if (DEBUG) {
            println(seq)
            println(node)
        }

        while (!seq.isEmpty) {

            if (DEBUG) {
                println(seq)
                println(node)
            }

            seq match {

                case '|' :: tail => {
                    val endNode = new Node(State.Other)

                    val alts = tail.head
                    seq = tail.tail

                    alts match {
                        case alts: List[Any] => {
                            for (altSeq <- alts) {
                                altSeq match {
                                    case altSeq: List[Any] => {
                                        val altNode = new Node(State.Other)
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

                    val inNode = new Node(State.Other)
                    val outNode = new Node(State.Other)

                    t match {
                        case c: Char => {
                            Parser.interpret(List(c), inNode).appendNext(outNode).appendNext(inNode)
                        }
                        case t: List[Any] => {
                            Parser.interpret(t, inNode).appendNext(outNode).appendNext(inNode)
                        }
                        case nil => nil
                    }

                    node.appendNext(outNode)
                    node.appendNext(inNode)

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
                            node = Parser.interpret(tail, node)
                        }
                        case nil => {
                        }
                    }

                    seq = tail
                }
                case nil => nil
            }
        }

        node
    }

    def interpretStr(str: String): Node = {
        null
    }

}


//Solution.main(args)

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

testInterpret()
