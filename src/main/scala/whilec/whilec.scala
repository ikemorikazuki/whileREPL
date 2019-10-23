package whilec

import lexer.WhileLexer
import parser.WhileParser
import lexer.WhileLexer
import parser.WhileParser

object Whilec {
  def main(args: Array[String]): Unit = {
    val code     = """x := 10;
              | y := 1;
              | while 10 <= x do
              |   y := y * x;
              |   x := x - 1;
              | end """.stripMargin
    val tokenSeq = WhileLexer.apply(code)
    for {
      tokens <- tokenSeq
      token  <- tokens
    } {
      println(token)
    }

    val code1  = "10 + 1 * 2;"
    val tokens = WhileLexer.apply(code1).right.get
    val ast    = WhileParser.apply(tokens)
    println(ast)
  }
}
