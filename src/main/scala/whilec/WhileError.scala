package whilec
import scala.collection.script.Location

sealed trait WhileError
case class WhileLexerError(location: Location, msg: String) extends WhileError

// 解析をしくじった位置を記録するための
case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
