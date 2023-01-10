package amyc.utils

import amyc.core.Context

object FileWriter {

  def apply(fileName: String)(body: => String)(using Context) =
    import java.io.{File, FileWriter}
    val fw = new FileWriter(new File(fileName))
    fw.write(body)
    fw.flush()

}
