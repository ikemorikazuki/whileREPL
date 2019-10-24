package eval

import org.scalatest._
import parser.WhileParser._
import parser._
import whilec.{Location, WhileParserError}
import lexer._
import eval.WhileEval._

class HelloSpec extends FlatSpec with Matchers {
  val state: Map[Var, Int] = Map()

  def parseAExp(tokens: Seq[WhileToken]): Either[WhileParserError, Aexp] = {
    val reader = new WhileTokenReader(tokens)
    aExpression(reader) match {
      case NoSuccess(msg, next) =>
        Left(WhileParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def parseBExp(tokens: Seq[WhileToken]): Either[WhileParserError, Bexp] = {
    val reader = new WhileTokenReader(tokens)
    bExpression(reader) match {
      case NoSuccess(msg, next) =>
        Left(WhileParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  // 算術式の評価のテスト
  "aEvalBigStep '1'" should "1" in {
    val code   = "1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens).right.get
    val exp    = aEvalBigStep(ast, state)
    exp shouldBe (1)
  }

  "aEvalBigStep '1 + 1'" should "2" in {
    val code   = "1 + 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens).right.get
    val exp    = aEvalBigStep(ast, state)
    exp shouldBe (2)
  }

  "aEvalBigStep '-1 + 1'" should "0" in {
    val code   = "-1 + 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens).right.get
    val exp    = aEvalBigStep(ast, state)
    exp shouldBe (0)
  }

  "aEvalBigStep '1 + 1 * 2'" should "3" in {
    val code   = "1 + 1 * 2"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens).right.get
    val exp    = aEvalBigStep(ast, state)
    exp shouldBe (3)
  }

  "aEvalBigStep '(1 + 1) * 2'" should "4" in {
    val code   = "(1 + 1) * 2"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens).right.get
    val exp    = aEvalBigStep(ast, state)
    exp shouldBe (4)
  }

  "aEvalBigStep '1 + 2 * (1 + 1)'" should "5" in {
    val code   = "1 + 2 * (1 + 1)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens).right.get
    val exp    = aEvalBigStep(ast, state)
    exp shouldBe (5)
  }

  // ブール式のテスト
  "bEvalBigStep 'true'" should "true" in {
    val code   = "true"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (true)
  }

  "bEvalBigStep 'false'" should "false" in {
    val code   = "false"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (false)
  }

  "bEvalBigStep '1 = 1'" should "true" in {
    val code   = "1 = 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (true)
  }

  "bEvalBigStep '1 + 1 = 1'" should "false" in {
    val code   = "1 + 1 = 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (false)
  }

  "bEvalBigStep '1 <= 1'" should "true" in {
    val code   = "1 <= 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (true)
  }

  "bEvalBigStep '1 + 1 <= 1'" should "false" in {
    val code   = "1 + 1 <= 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (false)
  }

  "bEvalBigStep 'true && true'" should "true" in {
    val code   = "true"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (true)
  }

  "bEvalBigStep '!(1 = 1)'" should "false" in {
    val code   = "!(1 = 1)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (false)
  }

  "bEvalBigStep '!(1 + 1 = 1)'" should "true" in {
    val code   = "!(1 + 1 = 1)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (true)
  }

  "bEvalBigStep '!(1 + 1 = 1) && false'" should "false" in {
    val code   = "!(1 + 1 = 1) && false"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens).right.get
    val exp    = bEvalBigStep(ast, state)
    exp shouldBe (false)
  }

  // statementの評価のテスト
  "evalBigStep 'x := 1;'" should "Map(x -> 1)" in {
    val code   = "x := 1;"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens).right.get
    val exp    = WhileEval.evalBigStep(ast, state)
    exp shouldBe (Map("x" -> 1))
  }

  "evalBigStep 'x := 1 + 3;'" should "Map(x -> 4)" in {
    val code   = "x := 1 + 3;"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens).right.get
    val exp    = WhileEval.evalBigStep(ast, state)
    exp shouldBe (Map("x" -> 4))
  }

  "evalBigStep 'x := 1 + 3 * 2;'" should "Map(x -> 7)" in {
    val code   = "x := 1 + 3 * 2;"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens).right.get
    val exp    = WhileEval.evalBigStep(ast, state)
    exp shouldBe (Map("x" -> 7))
  }

  "evalBigStep 'if true then skip else x := 1; end'" should "Map()" in {
    val code   = "if true then skip else x := 1; end"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens).right.get
    val exp    = WhileEval.evalBigStep(ast, state)
    exp shouldBe (Map())
  }

  "evalBigStep 'if false then skip else x := 1; end'" should "Map()" in {
    val code   = "if false then skip else x := 1; end"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens).right.get
    val exp    = WhileEval.evalBigStep(ast, state)
    exp shouldBe (Map("x" -> 1))
  }

  "evalBigStep 'x := 1; while 0 <= x do y := x + 1; x := x - 1; end'" should "Map(x -> 0, y -> 1)" in {
    val code   = "x := 1; while 0 <= x do y := x + 1; x := x - 1; end"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens).right.get
    val exp    = WhileEval.evalBigStep(ast, state)
    exp shouldBe (Map("x" -> -1, "y" -> 1))
  }
}
