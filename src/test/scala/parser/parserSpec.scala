package parser

import org.scalatest._
import lexer._
import parser.WhileParser._
import whilec.{Location, WhileParserError}

class LexerSpec extends FlatSpec with Matchers {
  val one   = NumExp(1)
  val two   = NumExp(2)
  val three = NumExp(3)
  val four  = NumExp(4)
  val ten   = NumExp(10)
  val x     = VarExp("x")
  val y     = VarExp("y")
  val tt    = BoolExp(true)
  val ff    = BoolExp(false)

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

  // 算術式のTEST
  "parse 1" should "NumExp(1)" in {
    val code   = "1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens)
    ast.right.get shouldBe (NumExp(1))
  }

  "parse 1 + 1" should "AddExp(1, 1)" in {
    val code   = "1 + 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens)
    ast.right.get shouldBe (AddExp(NumExp(1), NumExp(1)))
  }

  "parse 1 + 1 * 2" should s"Add($one, MulExp($one, $two))" in {
    val code   = "1 + 1 * 2"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens)
    ast.right.get shouldBe (AddExp(one, MulExp(one, two)))
  }

  // 前置演算子-は数字との間を開けてはならない
  "parse -1 - 1 * 2" should s"SubExp(NumExp(-1),MulExp($one, $two)" in {
    val code   = "-1 - 1 * 2"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens)
    ast.right.get shouldBe (SubExp(NumExp(-1), MulExp(one, two)))
  }

  "parse -1 - (1 * 2)" should s"SubExp(NumExp(-1),ParenExp(MulExp($one, $two))" in {
    val code   = "-1 - (1 * 2)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens)
    ast.right.get shouldBe (SubExp(NumExp(-1), ParenExp(MulExp(one, two))))
  }

  "parse -1 - (1 * 2) + 3" should s"AddExp(SubExp(NumExp(-1),ParenExp(MulExp($one,$two)),$three)" in {
    val code   = "-1 - (1 * 2) + 3"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens)
    ast.right.get shouldBe (AddExp(SubExp(NumExp(-1), ParenExp(MulExp(one, two))), three))
  }

  "parse x - (1 * 2) + 3" should s"AddExp(SubExp($x, ParenExp(MulExp($one, $two)), $three)" in {
    val code   = "x - (1 * 2) + 3"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseAExp(tokens)
    ast.right.get shouldBe (AddExp(SubExp(x, ParenExp(MulExp(one, two))), three))
  }

  // ブール式のTEST
  "parse true" should s"$tt" in {
    val code   = "true"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (tt)
  }

  "parse true" should s"$ff" in {
    val code   = "false"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (ff)
  }

  "parse 1 = 1" should s"$one = $one" in {
    val code   = "1 = 1"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (EqExp(NumExp(1), NumExp(1)))
  }

  "parse (1 + 1) = (3 + 3)" should s"EqExp(ParenExp(AddExp($one,$one),ParenExp(AddExp($three,$three))" in {
    val code   = "(1 + 1) =  (3 + 3)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (EqExp(ParenExp(AddExp(one, one)), ParenExp(AddExp(three, three))))
  }

  "parse (1 + 1) <= (3 + 3)" should s"EqExp(ParenExp(AddExp($one,$one),ParenExp(AddExp($three,$three))" in {
    val code   = "(1 + 1) <=  (3 + 3)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (LeExp(ParenExp(AddExp(one, one)), ParenExp(AddExp(three, three))))
  }

  "parse  true && false" should s"AndExp($tt, $ff)" in {
    val code   = "true && false"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (AndExp(tt, ff))
  }

  "parse  !true" should s"NotExp($tt))" in {
    val code   = "!true"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (NotExp(tt))
  }

  "parse !(false && true) && (1 = 1)" should s"AndExp(NotExp(BParen(AndExp($ff, $tt))), BParen(EqExp($one, $one)))" in {
    val code   = "!(false && true) && (1 = 1)"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = parseBExp(tokens)
    ast.right.get shouldBe (AndExp(NotExp(BParen(AndExp(ff, tt))), BParen(EqExp(one, one))))
  }

  "parse skip" should "SkipStm()" in {
    val code   = "skip"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (SkipStm())
  }

  "parse x := 1;" should s"AssignStm(x, $one) " in {
    val code   = "x := 1;"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (AssignStm("x", one))
  }

  "parse x := 1;y := 2" should s"SeqStm(AssignStm(x, $one), AssignStm(y, $two))" in {
    val code   = "x := 1;y := 2;"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (SeqStm(AssignStm("x", one), AssignStm("y", two)))
  }

  "parse if false then x := 1; else y := 1; end" should s"IfStm(ff, AssignStm(x, $one), AssignStm(y, $one))" in {
    val code   = "if false then x := 1; else y := 1; end"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (IfStm(ff, AssignStm("x", one), AssignStm("y", one)))
  }

  "parse while false do x := 1; end" should s"WhileStm($ff,AssignStm(x,$one)))" in {
    val code   = "while false do x := 1; end"
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (WhileStm(BoolExp(false), AssignStm("x", NumExp(1))))
  }

  "parse big statements" should s"Right(SeqStm(AssignStm(x,$ten),SeqStm(AssignStm(y,$one),WhileStm(LeExp($ten,$x),SeqStm(AssignStm(y,MulExp($y,$x))),AssignStm(x,SubExp($x,$one)))))))" in {
    val code   = """x := 10;
              | y := 1;
              | while 10 <= x do
              |   y := y * x; 
              |   x := x - 1;
              | end""".stripMargin
    val tokens = WhileLexer.apply(code).right.get
    val ast    = WhileParser.apply(tokens)
    ast.right.get shouldBe (
      SeqStm(
        AssignStm("x", ten),
        SeqStm(
          AssignStm("y", one),
          WhileStm(
            LeExp(ten, x),
            SeqStm(AssignStm("y", MulExp(y, x)), AssignStm("x", SubExp(x, one)))
          )
        )
      )
    )
  }
}
