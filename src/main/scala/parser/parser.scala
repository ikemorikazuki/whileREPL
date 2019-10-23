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
    expression(reader) match {
      case NoSuccess(msg, next) =>
        Left(WhileParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  // 数式の構文解析
  def expression: Parser[Aexp] = {
    (term ~ rep(ADD ~ term | SUB ~ term)) ^^ {
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

  def term: Parser[Aexp] = factor ~ rep(MUL ~ factor) ^^ {
    case opr1 ~ lists => {
      var operand1 = opr1
      lists.foreach { l =>
        l match {
          case MUL ~ f => { operand1 = MulExp(operand1, f) }
        }
      }
      operand1
    }
  }

  def factor: Parser[Aexp] =
    (LPAREN ~ expression ~ RPAREN) ^^ {
      case lparen ~ exp ~ rparen => ParenExp(exp)
    } | primary

  def primary: Parser[Aexp] = int ^^ { case INT(x) => NumExp(x) }
  lazy val int: Parser[INT] = acceptMatch("int", { case INT(x) => INT(x) })
}
