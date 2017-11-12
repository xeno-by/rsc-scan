package rsc.cli

import rsc.lexis._
import rsc.report._
import rsc.settings._
import rsc.tokenize._
import rsc.util._

object TokenizeMain {
  def main(args: Array[String]): Unit = {
    Settings.parse(args.toList) match {
      case Some(settings) =>
        val reporter = ConsoleReporter(settings)
        val inputs = settings.ins.map(in => Input(in))
        inputs.foreach { input =>
          if (input.file.isFile) {
            val tokenizer = Tokenizer(settings, reporter, input)
            println(input.str)
            try {
              while (tokenizer.token != EOF) {
                val msg_pos = s"[${tokenizer.start}..${tokenizer.end}) "
                val msg_token = s"${tokenRepl(tokenizer.token)} "
                val msg_data = {
                  val result = input.string.substring(tokenizer.start, tokenizer.end)
                  tokenizer.token match {
                    case COMMENT => result
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
                tokenizer.next()
              }
            } catch {
              case crash @ CrashException(pos, message, ex) =>
                val pos1 = if (pos != NoPosition) pos else reporter.pos
                val ex1 = if (ex != null) ex else crash
                reporter.append(CrashMessage(pos1, message, ex1))
              case ex: Throwable =>
                val pos = Position(input, tokenizer.offset, tokenizer.offset)
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
