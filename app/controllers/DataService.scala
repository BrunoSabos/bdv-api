package controllers

import java.io.{File, _}
import java.nio.charset.{Charset, CodingErrorAction}
import java.nio.file.Paths
import java.time.LocalDateTime

case class DataCache(var file: File, var date: LocalDateTime)

class DataService {
  def getFileContent(filepath: String): Option[String] = {
    val codec = new scala.io.Codec(Charset.defaultCharset())
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val file = new File(filepath)
    if(!file.exists()) {
      return Option.empty[String]
    }
    val source = scala.io.Source.fromFile(file)(codec)
    val lines = try source.mkString finally source.close()
    Some(lines)
  }

  def getCache(url: String, cachePath: String): Option[String] = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    val fileName = md.digest(url.getBytes("UTF-8")).map("%02x".format(_)).mkString
    val filePath = Paths.get(new File("public/cache").getAbsolutePath)+"/"+cachePath+"/"+fileName
    val content = getFileContent(filePath)
    if(content.nonEmpty){
      println(s"From cache: $url => $filePath")
    }
    content
  }

  def writeCache(url: String, content: String, cachePath: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    val fileName = md.digest(url.getBytes("UTF-8")).map("%02x".format(_)).mkString
    val filePath = Paths.get(new File("public/cache").getAbsolutePath)+"/"+cachePath+"/"+fileName
    val mbrWriter = new BufferedFileWriter(filePath)
    //        val mbrWriter = Files.newBufferedWriter(filePath))
    //        Files.write(Paths.get(filePath), json.getBytes(StandardCharsets.UTF_8))
    mbrWriter.write(content)
    mbrWriter.close()
    content
  }
}

class BufferedFileWriter(stream: OutputStream, charset: Charset) extends OutputStreamWriter(stream: OutputStream, charset: Charset) {
  def this(fileName: String) {
    this (new FileOutputStream(fileName), Charset.forName("UTF-8"))
  }

  def this(fileName: String, append: Boolean) {
    this (new FileOutputStream(fileName, append), Charset.forName("UTF-8"))
  }

  def this(fileName: String, charsetName: String, append: Boolean) {
    this (new FileOutputStream(fileName, append), Charset.forName(charsetName))
  }
  //
  //  def this(file: Nothing) {
  //    this (new FileOutputStream(file), Charset.forName("UTF-8"))
  //  }
  //
  //  def this(file: Nothing, append: Boolean) {
  //    this (new FileOutputStream(file, append), Charset.forName("UTF-8"))
  //  }
  //
  //  def this(file: Nothing, charsetName: String, append: Boolean) {
  //    this (new FileOutputStream(file, append), Charset.forName(charsetName))
  //  }
}