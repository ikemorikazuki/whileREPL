package eval

object WhileEval {
  import parser._
  type Var = String

  def aEvalBigStep(e: Aexp, state: Map[Var, Int]): Int = e match {
    case NumExp(i)      => i
    case VarExp(x)      => state.apply(x)
    case AddExp(e1, e2) => aEvalBigStep(e1, state) + aEvalBigStep(e2, state)
    case MulExp(e1, e2) => aEvalBigStep(e1, state) * aEvalBigStep(e2, state)
    case SubExp(e1, e2) => aEvalBigStep(e1, state) - aEvalBigStep(e2, state)
    case ParenExp(e)    => aEvalBigStep(e, state)
  }

  def bEvalBigStep(b: Bexp, state: Map[Var, Int]): Boolean = b match {
    case BoolExp(b)     => b
    case EqExp(e1, e2)  => aEvalBigStep(e1, state) == aEvalBigStep(e2, state)
    case LeExp(e1, e2)  => aEvalBigStep(e1, state) <= aEvalBigStep(e2, state)
    case NotExp(b)      => !bEvalBigStep(b, state)
    case AndExp(b1, b2) => bEvalBigStep(b1, state) && bEvalBigStep(b2, state)
    case BParen(b)      => bEvalBigStep(b, state)
  }

  def evalBigStep(s: Stm, state: Map[Var, Int]): Map[Var, Int] = s match {
    case AssignStm(x, e) => state.updated(x, aEvalBigStep(e, state))
    case SkipStm()       => state
    case SeqStm(s1, s2)  => evalBigStep(s2, evalBigStep(s1, state))
    case IfStm(b, s1, s2) =>
      if (bEvalBigStep(b, state)) {
        evalBigStep(s1, state)
      } else {
        evalBigStep(s2, state)
      }
    case WhileStm(b, s) => evalBigStep(IfStm(b, SeqStm(s, WhileStm(b, s)), SkipStm()), state)
  }
}
