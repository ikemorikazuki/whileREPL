package lexer

import scala.util.parsing.input.Positional

sealed trait WhileToken            extends Positional
case class IDENTIFIER(str: String) extends WhileToken // hoge, foo
case class INT(x: Int)             extends WhileToken // 1, 2
case object ASSIGN                 extends WhileToken // :=  演算子
case object ADD                    extends WhileToken // +
case object SUB                    extends WhileToken // -
case object MUL                    extends WhileToken // *
case object AND                    extends WhileToken // &&
case object NOT                    extends WhileToken // !
case object EQ                     extends WhileToken // =
case object LEQ                    extends WhileToken // <=
case object SEMICOLON              extends WhileToken // ;  デリミタ
case object LPAREN                 extends WhileToken // "("
case object RPAREN                 extends WhileToken // ")"
case object WHILE                  extends WhileToken // keyword while
case object DO                     extends WhileToken // do
case object END                    extends WhileToken // end
case object SKIP                   extends WhileToken // skip
case object IF                     extends WhileToken // if
case object THEN                   extends WhileToken // then
case object ELSE                   extends WhileToken // else
case object TRUE                   extends WhileToken // true
case object FALSE                  extends WhileToken // false
