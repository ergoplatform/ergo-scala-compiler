package org.ergoplatform.compiler.compilation

import scala.reflect.macros.whitebox.{Context => MacroContext}

object util {

  implicit class RichContext(c: MacroContext) {

    def error(msg: String): Unit =
      c.error(c.enclosingPosition, msg)

    def fail(msg: String): Nothing =
      c.abort(c.enclosingPosition, msg)

    def warn(msg: String): Unit =
      c.warning(c.enclosingPosition, msg)

    def info(msg: String): Unit =
      c.info(c.enclosingPosition, msg, force = true)

    def debug[T](v: T): T = {
      info(v.toString)
      v
    }
  }

}
