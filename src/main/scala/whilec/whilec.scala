package whilec

import lexer.WhileLexer._

object Whilec {
  def main(args: Array[String]): Unit = {
    val code     = """x := 10;
              | y := 1;
              | while 10 <= x do
              |   y := y * x;
              |   x := x - 1;
              | end """.stripMargin
    val tokenSeq = apply(code)
    println(tokenSeq)
  }
}
