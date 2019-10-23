package whilec

import lexer.WhileLexer
import parser.WhileParser
import lexer._
import parser.WhileParser._
import parser._
import whilec.{Location, WhileParserError}

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

    def parseBExp(tokens: Seq[WhileToken]): Either[WhileParserError, Bexp] = {
      val reader = new WhileTokenReader(tokens)
      bExpression(reader) match {
        case NoSuccess(msg, next) =>
          Left(WhileParserError(Location(next.pos.line, next.pos.column), msg))
        case Success(result, next) => Right(result)
      }
    }

    val code1  = "true && false"
    val tokens = WhileLexer.apply(code1).right.get
    val ast    = parseBExp(tokens)
    println(ast)

  }
}
