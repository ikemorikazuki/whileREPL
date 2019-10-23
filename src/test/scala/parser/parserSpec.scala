package parser

import org.scalatest._
import lexer.WhileLexer

class LexerSpec extends FlatSpec with Matchers {
  val one   = NumExp(1)
  val two   = NumExp(2)
  val three = NumExp(3)
  val four  = NumExp(4)
  // 算術式のTEST
  "parse 1" should "NumExp(1)" in {
    val code   = "1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (NumExp(1))
  }

  "parse 1 + 1" should "AddExp(1, 1)" in {
    val code   = "1 + 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (AddExp(NumExp(1), NumExp(1)))
  }

  "parse 1 + 1 * 2" should s"Add($one, MulExp($one, $two))" in {
    val code   = "1 + 1 * 2"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (AddExp(one, MulExp(one, two)))
  }

  // 前置演算子-は数字との間を開けてはならない
  "parse -1 - 1 * 2" should s"SubExp(NumExp(-1),MulExp($one, $two)" in {
    val code   = "-1 - 1 * 2"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (SubExp(NumExp(-1), MulExp(one, two)))
  }

  "parse -1 - (1 * 2)" should s"SubExp(NumExp(-1),ParenExp(MulExp($one, $two))" in {
    val code   = "-1 - (1 * 2)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (SubExp(NumExp(-1), ParenExp(MulExp(one, two))))
  }

  "parse -1 - (1 * 2) + 3" should s"AddExp(SubExp(NumExp(-1),ParenExp(MulExp($one,$two)),$three)" in {
    val code   = "-1 - (1 * 2) + 3"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (AddExp(SubExp(NumExp(-1), ParenExp(MulExp(one, two))), three))
  }
}
