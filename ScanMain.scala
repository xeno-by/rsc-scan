package rsc.cli

import rsc.lexis._
import rsc.report._
import rsc.scan._
import rsc.settings._
import rsc.util._

object ScanMain {
  def main(args: Array[String]): Unit = {
    Settings.parse(args.toList) match {
      case Some(settings) =>
        val reporter = ConsoleReporter(settings)
        val inputs = settings.ins.map(in => Input(in))
        inputs.foreach { input =>
          if (input.file.isFile) {
            val scanner = Scanner(settings, reporter, input)
            println(input.str)
            try {
              var prevEnd = 0
              while (scanner.token != EOF) {
                if (scanner.start != prevEnd) {
                  sys.error(s"Token start (${scanner.start}) doesn't match previous token end ($prevEnd)")
                } else {
                  prevEnd = scanner.end
                }
                val msg_pos = s"[${scanner.start}..${scanner.end}) "
                val msg_token = s"${tokenRepl(scanner.token)} "
                val msg_value = if (scanner.value != null) scanner.value else ""
                println(msg_pos + msg_token + msg_value)
                scanner.next()
              }
              if (scanner.end != input.string.length) {
                sys.error(s"Token stream end (${scanner.end}) doesn't match input length (${input.string.length})")
              }
            } catch {
              case crash @ CrashException(pos, message, ex) =>
                val pos1 = if (pos != NoPosition) pos else reporter.pos
                val ex1 = if (ex != null) ex else crash
                reporter.append(CrashMessage(pos1, message, ex1))
              case ex: Throwable =>
                val pos = Position(input, scanner.offset, scanner.offset)
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
