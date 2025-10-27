package scinear

inline val NONE = 0
inline val ERROR = 1
inline val WARN = 2
inline val INFO = 3
inline val DEBUG = 4

type LogLevel = Int

object logger:
  inline val configLevel = NONE

  private inline def log(msg: => String, inline level: LogLevel): Unit =
    inline if configLevel >= level then println(s"[${level}] $msg")

  inline def info(msg: => String): Unit =
    log(msg, INFO)

  inline def debug(msg: => String): Unit =
    log(msg, DEBUG)

  inline def warn(msg: => String): Unit =
    log(msg, WARN)

  inline def error(msg: => String): Unit =
    log(msg, ERROR)

  inline def debug[T](msg: => String)(inline op: T): T =
    log("===>" + msg, DEBUG)
    val ret = op
    log("<===" + msg + " with " + ret, DEBUG)
    ret

end logger
