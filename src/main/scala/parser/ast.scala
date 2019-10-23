package parser

import scala.util.parsing.input.Positional

sealed trait Aexp                     extends Positional
case class NumExp(i: Int)             extends Aexp
case class VarExp(s: String)          extends Aexp
case class AddExp(e1: Aexp, e2: Aexp) extends Aexp
case class MulExp(e1: Aexp, e2: Aexp) extends Aexp
case class SubExp(e1: Aexp, e2: Aexp) extends Aexp
case class ParenExp(e: Aexp)          extends Aexp

sealed trait Bexp                     extends Positional
case class BoolExp(b: Boolean)        extends Bexp
case class EqExp(e1: Aexp, e2: Aexp)  extends Bexp
case class LeExp(e1: Aexp, e2: Aexp)  extends Bexp // less than or equal
case class NotExp(b: Bexp)            extends Bexp
case class AndExp(b1: Bexp, b2: Bexp) extends Bexp
case class BParen(b: Bexp)            extends Bexp

sealed trait Stm                            extends Positional
case class AssignStm(x: String, e: Aexp)    extends Stm
case class SkipStm()                        extends Stm
case class SeqStm(s1: Stm, s2: Stm)         extends Stm
case class IfStm(b: Bexp, s1: Stm, s2: Stm) extends Stm
case class WhileStm(b: Bexp, s: Stm)        extends Stm
