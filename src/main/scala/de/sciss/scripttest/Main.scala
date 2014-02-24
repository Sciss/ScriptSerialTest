package de.sciss.scripttest

import scala.tools.nsc
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream}
import scala.annotation.tailrec

object Main extends App {
  val exampleSource = """println("Hello from Foo")"""

  val packageName = "de.sciss.mellite.user"

  private var userCount = 0

  def mkFunName(): String = {
    val c = userCount
    userCount += 1
    s"Fun$c"
  }

  def wrapSource(source: String): (String, String) = {
    val fun = mkFunName()
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
    val (fun, code)     = wrapSource(source)
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
  def packJar(base: File): Array[Byte] = {
    import java.util.jar._

    val mf = new Manifest
    mf.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    val bs    = new java.io.ByteArrayOutputStream
    val out   = new JarOutputStream(bs, mf)

    def add(prefix: String, f: File): Unit = {
      val name0 = prefix + f.getName
      val name  = if (f.isDirectory) name0 + "/" else name0
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
      if (f.isDirectory) f.listFiles.foreach(add(name, _))
    }

    base.listFiles().foreach(add("", _))
    out.close()
    bs.toByteArray
  }

  def unpackJar(bytes: Array[Byte]): Map[String, Array[Byte]] = {
    import java.util.jar._
    import scala.annotation.tailrec

    val in = new JarInputStream(new ByteArrayInputStream(bytes))
    val b  = Map.newBuilder[String, Array[Byte]]

    @tailrec def loop(): Unit = {
      val entry = in.getNextJarEntry
      if (entry != null) {
        if (!entry.isDirectory) {
          val name  = entry.getName
          // val sz    = entry.getSize.toInt
          // println(s"name = '$name', size = $sz")

          // cf. http://stackoverflow.com/questions/8909743/jarentry-getsize-is-returning-1-when-the-jar-files-is-opened-as-inputstream-f
          val bs  = new ByteArrayOutputStream
          var i   = 0
          while (i >= 0) {
            i = in.read()
            if (i >= 0) bs.write(i)
          }
          val bytes = bs.toByteArray
          b += name -> bytes
        }
        loop()
      }
    }
    loop()
    in.close()
    b.result()
  }

  locally {
    println("Compiling...")
    val (fun, bytes) = compile(exampleSource)

    //    val out = new FileOutputStream(new File(new File(sys.props("user.home"), "Desktop"), "test.jar"))
    //    out.write(bytes)
    //    out.close()

    val map = unpackJar(bytes)
    println("Map contents:")
    map.keys.foreach(k => println(s"  '$k'"))

    val map1    = map.map { case (key, value) => mkClassName(key) -> value }

    val cl      = new MemoryJarClassLoader(map1)
    test(fun, cl)   // should call `defineClass`
    test(fun, cl)   // should find cached class
  }

  def test(fun: String, cl: ClassLoader): Unit = {
    val clName  = s"$packageName.$fun"
    println(s"Resolving class '$clName'...")
    val clazz = Class.forName(clName, true, cl)
    println("Instantiating...")
    val x     = clazz.newInstance().asInstanceOf[() => Unit]
    println("Invoking 'apply':")
    x()
  }

  /** Converts a jar entry name with slashes to a class name with dots
    * and dropping the `class` extension
    */
  def mkClassName(path: String): String = {
    require(path.endsWith(".class"))
    path.substring(0, path.length - 6).replace("/", ".")
  }

  class MemoryJarClassLoader(map: Map[String, Array[Byte]]) extends ClassLoader {
    override protected def findClass(name: String): Class[_] =
      map.get(name).map { bytes =>
        println(s"defineClass(\"$name\", ...)")
        defineClass(name, bytes, 0, bytes.length)

      } .getOrElse(super.findClass(name)) // throws exception
  }
}
