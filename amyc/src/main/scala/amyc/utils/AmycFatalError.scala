package amyc.utils

case class AmycFatalError(msg: String) extends Exception(msg)
