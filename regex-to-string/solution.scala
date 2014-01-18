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

class Node(state: State.Value, char: Char) {

    Global.counter += 1

    val id = Global.counter

    val nexts = new ListBuffer[Node]()

    def appendNext(node: Node) = {
        this.nexts += node

        this
    }
}

object Parser {

    def parse(chrs: List[Char]): (Stack[Any], List[Char]) = {

        var chars = chrs

        var seq = new Stack[Any]()
        val alt = new Stack[Any]()

        while (!chars.isEmpty) {
            println(chars)
            println(seq)

            chars match {
                case '(' :: tail => {
                    val (parsed, rest) = parse(tail)
                    seq.push(parsed)
                    chars = rest
                }
                case ')' :: tail => {

                    if (alt.length > 0) {
                        alt.push(seq)

                        val result = new Stack[Any]()
                        result.push('|', alt)

                        return (result, chars.tail)
                    }

                    return (seq, chars.tail)
                }
                case '*' :: tail => {
                    val last = seq.pop()
                    seq.push('*', last)
                    chars = chars.tail
                }
                case '|' :: tail => {
                    alt.push(seq)
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
            alt.push(seq)

            val result = new Stack[Any]()
            result.push('|', alt)

            return (result, chars)
        }

        (seq, chars)
    }

    def interpret() = {}

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

testParse()
