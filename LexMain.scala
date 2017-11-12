package rsc.cli

import rsc.lex._
import rsc.lexis._
import rsc.report._
import rsc.settings._
import rsc.util._

object LexMain {
  def main(args: Array[String]): Unit = {
    Settings.parse(args.toList) match {
      case Some(settings) =>
        val reporter = ConsoleReporter(settings)
        val inputs = settings.ins.map(in => Input(in))
        inputs.foreach { input =>
          if (input.file.isFile) {
            val lexer = Lexer(settings, reporter, input)
            println(input.str)
            try {
              var prevEnd = 0
              while (lexer.token != EOF) {
                if (lexer.start != prevEnd) {
                  sys.error(s"Token start (${lexer.start}) doesn't match previous token end ($prevEnd)")
                } else {
                  prevEnd = lexer.end
                }
                val msg_pos = s"[${lexer.start}..${lexer.end}) "
                val msg_token = s"${tokenRepl(lexer.token)} "
                val msg_data = {
                  val result = input.string.substring(lexer.start, lexer.end)
                  lexer.token match {
                    case LITCHAR => result
                    case LITDOUBLE => result
                    case LITFLOAT => result
                    case LITINT => result
                    case LITLONG => result
                    case LITSTRING => result
                    case LITSYMBOL => result
                    case ID => result
                    case _ => ""
                  }
                }
                println(msg_pos + msg_token + msg_data)
                lexer.next()
              }
              if (lexer.end != input.string.length) {
                sys.error(s"Token stream end (${lexer.end}) doesn't match input length (${input.string.length})")
              }
            } catch {
              case crash @ CrashException(pos, message, ex) =>
                val pos1 = if (pos != NoPosition) pos else reporter.pos
                val ex1 = if (ex != null) ex else crash
                reporter.append(CrashMessage(pos1, message, ex1))
              case ex: Throwable =>
                val pos = Position(input, lexer.offset, lexer.offset)
                reporter.append(CrashMessage(pos, ex.getMessage, ex))
            }
          } else {
            reporter.append(FileNotFound(input))
            None
          }
        }
        if (reporter.problems.nonEmpty) sys.exit(1)
        else sys.exit(0)
      case None =>
        sys.exit(1)
    }
  }
}
