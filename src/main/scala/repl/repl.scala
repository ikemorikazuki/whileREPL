package repl

import lexer.WhileLexer
import parser.WhileParser
import eval.WhileEval._
import io.AnsiColor._

object WhileREPL {
  val REPL = "While> "
  def start(): Unit = {
    var state: Map[String, Int] = Map()
    var judge                   = true
    while (judge) {
      print(s"${RESET}${BOLD}${GREEN}${REPL}${RESET}")
      val input = io.StdIn.readLine()

      input match {
        case "reset state" => {
          state = Map()
        }
        case "quit" | ":q" => {
          judge = false
        }

        case _ => {
          WhileLexer.apply(input) match {
            case Left(lexerErr) => println(lexerErr)
            case Right(tokens) => {
              WhileParser.apply(tokens) match {
                case Left(parserErr) =>
                  println(s"${RESET}${BOLD}${RED}ERROR${RESET}: ${parserErr}\n")
                case Right(ast) => {
                  state = evalBigStep(ast, state)
                  println(s"${RESET}${BOLD}${CYAN}state${RESET} = ${state}\n")
                }
              }
            }
          }
        }
      }
    }
  }
}
