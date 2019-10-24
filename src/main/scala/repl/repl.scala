package repl

import lexer.WhileLexer
import parser.WhileParser
import eval.WhileEval._
import io.AnsiColor._
import jline.console.ConsoleReader

object WhileREPL {
  val REPL = "While> "
  def start(): Unit = {
    var state: Map[String, Int] = Map()
    var judge                   = true
    val jline                   = new ConsoleReader()
    jline.setExpandEvents(false)
    jline.setPrompt(s"${RESET}${BOLD}${GREEN}${REPL}${RESET}")
    while (judge) {
      val input = jline.readLine()
      input match {
        case ":quit" | ":q" => {
          judge = false
          println(s"finish While REPL. ${RESET}${BOLD}${YELLOW}SEE YOU AGAIN!${RESET}")
          return
        }
        case "state" => println(s"${RESET}${BOLD}${CYAN}state${RESET} = ${state}\n")
        case "state.fresh" => {
          state = Map()
        }
        case _ => {
          WhileLexer.apply(input) match {
            case Left(lexerErr) => println(s"${RESET}${BOLD}${RED}ERROR${RESET}: ${lexerErr}\n")
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
