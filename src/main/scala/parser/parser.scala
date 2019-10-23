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
  def apply(tokens: Seq[WhileToken]): Either[WhileParserError, Aexp] = {
    val reader = new WhileTokenReader(tokens)
    aExpression(reader) match {
      case NoSuccess(msg, next) =>
        Left(WhileParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  // 数式の構文解析
  def aExpression: Parser[Aexp] = {
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

  def aterm: Parser[Aexp] = {
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

  def factor: Parser[Aexp] =
    (LPAREN ~ aExpression ~ RPAREN) ^^ {
      case lparen ~ exp ~ rparen => ParenExp(exp)
    } | primary

  def primary: Parser[Aexp] = int ^^ { case INT(x) => NumExp(x) } | ident ^^ {
    case IDENTIFIER(v) => VarExp(v)
  }
  lazy val int: Parser[INT]          = acceptMatch("int", { case INT(x)          => INT(x) })
  lazy val ident: Parser[IDENTIFIER] = acceptMatch("ident", { case IDENTIFIER(v) => IDENTIFIER(v) })
  // ブール式の構文解析

  def bExpression: Parser[Bexp] = {
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

  def bterm: Parser[Bexp] = {
    (opt(NOT) ~ bfactor) ^^ {
      case None ~ f    => f
      case Some(_) ~ f => NotExp(f)
    }
  }

  def bfactor: Parser[Bexp] =
    (LPAREN ~ bExpression ~ RPAREN) ^^ {
      case lparen ~ bexp ~ rparen => BParen(bexp)
    } | eq | leq | True | False

  def eq: Parser[Bexp] = {
    (aExpression ~ EQ ~ aExpression) ^^ {
      case aex1 ~ EQ ~ aex2 => EqExp(aex1, aex2)
    }
  }

  def leq: Parser[Bexp] = {
    (aExpression ~ LEQ ~ aExpression) ^^ {
      case aex1 ~ LEQ ~ aex2 => LeExp(aex1, aex2)
    }
  }

  def True: Parser[Bexp] = TRUE ^^ { _ =>
    BoolExp(true)
  }

  def False: Parser[Bexp] = FALSE ^^ { _ =>
    BoolExp(false)
  }

}
