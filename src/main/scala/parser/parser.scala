package parser
import scala.util.parsing.combinator.Parsers
import whilec.{Location, WhileParserError}
import lexer._
import scala.util.parsing.input.{Position, NoPosition, Reader}

object WhileParser extends Parsers {
  override type Elem = WhileToken

  // Whiletoken列を読み込むためのオーバーライド
  class WhileTokenReader(tokens: Seq[WhileToken]) extends Reader[WhileToken] {
    override def first: WhileToken        = tokens.head
    override def atEnd: Boolean           = tokens.isEmpty
    override def pos: Position            = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[WhileToken] = new WhileTokenReader(tokens.tail)
  }

  //applyメソッドの実装
  def apply(tokens: Seq[WhileToken]): Either[WhileParserError, Stm] = {
    val reader = new WhileTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) =>
        Left(WhileParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[Stm] = {
    phrase(block)
  }

  // 数式の構文解析
  def aExpression: Parser[Aexp] = positioned {
    (aterm ~ rep(ADD ~ aterm | SUB ~ aterm)) ^^ {
      case opr1 ~ lists => {
        var operand1 = opr1
        lists.foreach(
          l =>
            l match {
              case ADD ~ f => { operand1 = AddExp(operand1, f) }
              case SUB ~ f => { operand1 = SubExp(operand1, f) }
            }
        )
        operand1
      }
    }
  }

  def aterm: Parser[Aexp] = positioned {
    factor ~ rep(MUL ~ factor) ^^ {
      case opr1 ~ lists => {
        var operand1 = opr1
        lists.foreach(
          l =>
            l match {
              case MUL ~ f => { operand1 = MulExp(operand1, f) }
            }
        )
        operand1
      }
    }
  }

  def factor: Parser[Aexp] = positioned {
    (LPAREN ~ aExpression ~ RPAREN) ^^ {
      case lparen ~ exp ~ rparen => ParenExp(exp)
    } | primary
  }

  def primary: Parser[Aexp] = positioned {
    int ^^ { case INT(x) => NumExp(x) } | ident ^^ {
      case IDENTIFIER(v) => VarExp(v)
    }
  }

  lazy val int: Parser[INT] = positioned {
    acceptMatch("int", { case INT(x) => INT(x) })
  }

  lazy val ident: Parser[IDENTIFIER] = positioned {
    acceptMatch("ident", { case IDENTIFIER(v) => IDENTIFIER(v) })
  }

  // ブール式の構文解析
  def bExpression: Parser[Bexp] = positioned {
    (bterm ~ rep(AND ~ bterm)) ^^ {
      case bexp ~ lists => {
        var op = bexp
        lists.foreach(
          l => {
            l match {
              case AND ~ f => { op = AndExp(op, f) }
            }
          }
        )
        op
      }
    }
  }

  def bterm: Parser[Bexp] = positioned {
    (opt(NOT) ~ bfactor) ^^ {
      case None ~ f    => f
      case Some(_) ~ f => NotExp(f)
    }
  }

  def bfactor: Parser[Bexp] = positioned {
    (LPAREN ~ bExpression ~ RPAREN) ^^ {
      case lparen ~ bexp ~ rparen => BParen(bexp)
    } | eq | leq | True | False
  }

  def eq: Parser[Bexp] = positioned {
    (aExpression ~ EQ ~ aExpression) ^^ {
      case aex1 ~ EQ ~ aex2 => EqExp(aex1, aex2)
    }
  }

  def leq: Parser[Bexp] = positioned {
    (aExpression ~ LEQ ~ aExpression) ^^ {
      case aex1 ~ LEQ ~ aex2 => LeExp(aex1, aex2)
    }
  }

  def True: Parser[Bexp] = positioned {
    TRUE ^^ { _ =>
      BoolExp(true)
    }
  }
  def False: Parser[Bexp] = positioned {
    FALSE ^^ { _ =>
      BoolExp(false)
    }
  }

  // statementの構文解析
  def block: Parser[Stm] = positioned {
    rep1(statement) ^^ { case stmlist => stmlist.reduceRight(SeqStm) }
  }

  def statement: Parser[Stm] = positioned {
    val skip = positioned { SKIP ^^ (_ => SkipStm()) }
    val assign = positioned {
      ident ~ ASSIGN ~ aExpression ~ SEMICOLON ^^ {
        case IDENTIFIER(i) ~ ASSIGN ~ aExpression ~ SEMICOLON => AssignStm(i, aExpression)
      }
    }
    skip | assign | ifThen | whileDo
  }

  def ifThen: Parser[Stm] = positioned {
    (IF ~ bExpression ~ THEN ~ block ~ ELSE ~ block ~ END) ^^ {
      case IF ~ bex ~ THEN ~ stms1 ~ ELSE ~ stms2 ~ END => IfStm(bex, stms1, stms2)
    }
  }

  def whileDo: Parser[Stm] = positioned {
    (WHILE ~ bExpression ~ DO ~ block ~ END) ^^ {
      case WHILE ~ bex ~ DO ~ stms ~ END => WhileStm(bex, stms)
    }
  }
}
