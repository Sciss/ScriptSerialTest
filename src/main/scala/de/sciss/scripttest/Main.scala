package de.sciss.scripttest

import scala.tools.nsc
import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream}
import scala.annotation.tailrec

object Main extends App {
  val exampleSource = """println("Hello from Foo")"""

  private val packageName = "de.sciss.mellite.user"

  private var userCount = 0

  private def funName(): String = {
    val c = userCount
    userCount += 1
    s"Fun$c"
  }

  private def wrap(source: String): (String, String) = {
    val fun = funName()
    val code = s"""package $packageName
                  |
                  |class $fun extends Function0[Unit] {
                  |  def apply(): Unit = {
                  |    $source
                  |  }
                  |}
                  |""".stripMargin
    (fun, code)
  }

  /** Compiles a source code consisting of a body which is wrapped in a `Function0` apply method,
    * and returns the function's class name (without package) and the raw jar file produced in the compilation.
    */
  def compile(source: String): (String, Array[Byte]) = {
    val set             = new nsc.Settings
    val d               = File.createTempFile("temp", ".out")
    d.delete(); d.mkdir()
    set.d.value         = d.getPath
    set.usejavacp.value = true
    val compiler        = new nsc.Global(set)
    val f               = File.createTempFile("temp", ".scala")
    val out             = new BufferedOutputStream(new FileOutputStream(f))
    val (fun, code)     = wrap(source)
    out.write(code.getBytes("UTF-8"))
    out.flush(); out.close()
    val run             = new compiler.Run()
    run.compile(List(f.getPath))
    f.delete()

    val bytes = packJar(d)
    deleteDir(d)

    (fun, bytes)
  }

  private def deleteDir(base: File): Unit = {
    base.listFiles().foreach { f =>
      if (f.isFile) f.delete()
      else deleteDir(f)
    }
    base.delete()
  }

  // cf. http://stackoverflow.com/questions/1281229/how-to-use-jaroutputstream-to-create-a-jar-file
  private def packJar(base: File): Array[Byte] = {
    import java.util.jar._

    val mf = new Manifest
    mf.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    val bs    = new java.io.ByteArrayOutputStream
    val out   = new JarOutputStream(bs, mf)

    def add(f: File): Unit = {
      val name0 = f.getPath.replace("\\", "/")
      val name  = if (f.isDirectory && !name0.endsWith("/")) name0 + "/" else name0
      val entry = new JarEntry(name)
      entry.setTime(f.lastModified())
      // if (f.isFile) entry.setSize(f.length())
      out.putNextEntry(entry)
      if (f.isFile) {
        val in = new BufferedInputStream(new FileInputStream(f))
        try {
          val buf = new Array[Byte](1024)
          @tailrec def loop(): Unit = {
            val count = in.read(buf)
            if (count >= 0) {
              out.write(buf, 0, count)
              loop()
            }
          }
          loop()
        } finally {
          in.close()
        }
      }
      out.closeEntry()
      if (f.isDirectory) f.listFiles.foreach(add)
    }

    add(base)
    out.close()
    bs.toByteArray
  }

  def unpackJar(bytes: Array[Byte]): Map[String, Array[Byte]] = {
    import java.util.jar._
    import scala.annotation.tailrec

    val in = new JarInputStream(new java.io.ByteArrayInputStream(bytes))
    val b  = Map.newBuilder[String, Array[Byte]]

    @tailrec def loop(): Unit = {
      val entry = in.getNextJarEntry
      if (entry != null) {
        if (!entry.isDirectory) {
          val name  = entry.getName
          val sz    = entry.getSize.toInt
          println(s"name = '$name', size = $sz")
          if (sz >= 0) {
            val bytes = new Array[Byte](sz)
            in.read(bytes)
            b += name -> bytes
          }
        }
        loop()
      }
    }
    loop()
    in.close()
    b.result()
  }

  //  class MemoryJarClassLoader(bytes: Array[Byte]) extends ClassLoader {
  //    override protected def findClass(name: String): Class[_] = {
  //      val bytes = map.getOrElse(name, sys.error(s"Function '$name' not defined"))
  //      defineAll(name, bytes)
  //      ???
  //    }
  //  }

  locally {
    println("Compiling...")
    val (fun, bytes) = compile(exampleSource)

    val out = new FileOutputStream(new File(new File(sys.props("user.home"), "Desktop"), "test.jar"))
    out.write(bytes)
    out.close()

    val map = unpackJar(bytes)
    println("Map contents:")
    map.keys.foreach(k => println(s"  '$k'"))
  }
}
