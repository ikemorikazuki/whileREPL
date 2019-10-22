package lexer

import scala.util.parsing.combinator.RegexParsers
import whilec.{Location, WhileLexerError}

object WhileLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace     = """[\t\r\n" "]+""".r

  //
  def apply(code: String): Either[WhileLexerError, List[WhileToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) =>
        Left(WhileLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  // 繰り返し文字列を解析しtoken列を生成する
  def tokens: Parser[List[WhileToken]] = {
    phrase(
      rep1(
        int
          | assign
          | add
          | sub
          | mul
          | and
          | not
          | eq
          | leq
          | semicolon
          | lparen
          | rparen
          | While
          | Do
          | end
          | skip
          | If
          | Then
          | Else
          | True
          | False
          | identifier
      )
    )
  }

  // 変数は 英小文字と英大文字と数字の組み合わせで英司から始まる。
  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str =>
      IDENTIFIER(str)
    }
  }

  // 数字は整数のみ
  def int: Parser[INT] = positioned {
    """(-)?(\d+)(\.\d*)?""".r ^^ { lit =>
      INT(lit.toInt)
    }
  }

  // 演算子

  def assign    = positioned { ":=" ^^ (_ => ASSIGN) }   // ASSIGN
  def add       = positioned { "+" ^^ (_ => ADD) }       // ADD
  def sub       = positioned { "-" ^^ (_ => SUB) }       // SUB
  def mul       = positioned { "*" ^^ (_ => MUL) }       // MUL
  def and       = positioned { "&&" ^^ (_ => AND) }      // AND
  def not       = positioned { "!" ^^ (_ => NOT) }       // NOT
  def eq        = positioned { "=" ^^ (_ => EQ) }        // EQ
  def leq       = positioned { "<=" ^^ (_ => LEQ) }      // LEQ
  def semicolon = positioned { ";" ^^ (_ => SEMICOLON) } // SEMICOLON
  def lparen    = positioned { "(" ^^ (_ => LPAREN) }    // LPAREN
  def rparen    = positioned { ")" ^^ (_ => RPAREN) }    // RPAREN
  def While     = positioned { "while" ^^ (_ => WHILE) } // WHILE
  def Do        = positioned { "do" ^^ (_ => DO) }       // DO
  def end       = positioned { "end" ^^ (_ => END) }     // END
  def skip      = positioned { "skip" ^^ (_ => SKIP) }   // SKIP
  def If        = positioned { "if" ^^ (_ => IF) }       // IF
  def Then      = positioned { "then" ^^ (_ => THEN) }   // THEN
  def Else      = positioned { "else" ^^ (_ => ELSE) }   // ELSE
  def True      = positioned { "true" ^^ (_ => TRUE) }   // TRUE
  def False     = positioned { "false" ^^ (_ => FALSE) } //FALSE
}
