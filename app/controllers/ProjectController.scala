package controllers

import java.nio.charset.StandardCharsets
import javax.inject._

import org.neo4j.driver.v1.{AuthTokens, GraphDatabase}
import play.api.Configuration
import play.api.http.ContentTypes
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.mvc._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex
import scala.collection.JavaConverters._

@Singleton
class ProjectController @Inject()(cc: ControllerComponents, config: Configuration, ws: WSClient) extends AbstractController(cc) {

  implicit class RichJsObject(original: JsObject) {
    def omitEmpty: JsObject = original.value.foldLeft(original) {
      case (obj, (key, JsString(st))) if st == null || st.isEmpty => obj - key
      case (obj, (key, JsArray(arr))) if arr == null || arr.isEmpty => obj - key
      case (obj, (_, _)) => obj
    }
  }

  implicit val gNodehWrites = new Writes[GNode] {
    def writes(n: GNode) = Json.obj(
      "label" -> n.label,
      "name" -> n.name
    ).omitEmpty
  }

  implicit val gLinkWrites = new Writes[GLink] {
    def writes(n: GLink) = Json.obj(
      "label" -> n.label,
      "from" -> n.from,
      "to" -> n.to
    ).omitEmpty
  }

  implicit val gGraphWrites = new Writes[GGraph] {
    def writes(g: GGraph) = Json.obj(
      "nodes" -> g.nodes,
      "links" -> g.links
//      "nodes" -> Json.arr(
//        g.nodes
//      )
    ).omitEmpty
  }

  implicit val responseUserReads: Reads[ResponseUser] = (
    (__ \ "id").read[Int] and
      (__ \ "name").read[String] and
      (__ \ "username").read[String]
    ) (ResponseUser.apply _)

  implicit val responseUserProjectReads: Reads[ResponseUserProject] = (
    (__ \ "id").read[Int] and
      (__ \ "name").read[String]
    ) (ResponseUserProject.apply _)

  implicit val responseProjectReads: Reads[ResponseProject] = (
    (__ \ "id").read[Int] and
      (__ \ "name").read[String] and
      (__ \ "path_with_namespace").read[String]
    ) (ResponseProject.apply _)

  implicit val responseProjectCommitsReads: Reads[ResponseProjectCommit] = (
    (__ \ "id").read[String] and
      (__ \ "author_name").read[String] and
      (__ \ "author_email").read[String]
    ) (ResponseProjectCommit.apply _)

  //  implicit val backupsReportReads: Reads[BackupsReport] =
  //    (__ \ "clients").read[Seq[BackupClientReport]].map(c => BackupsReport(c))

  def list() = Action { implicit request: Request[AnyContent] => {

//    val gitHost = config.underlying.getString("sources.git.host")
//    val gitToken = config.underlying.getString("sources.git.token")

    val driver = GraphDatabase.driver("bolt://127.0.0.1:7687", AuthTokens.basic("bruno", "bruno"))

    val session = driver.session
    val allJson = session.run("MATCH (u)-[r]->(p) RETURN u.username as username, type(r) as type, p.name as name")
    var personsJson: Seq[GNode] = Seq[GNode]()
    var projectsJson: Seq[GNode] = Seq[GNode]()
    var linksJson: Seq[GLink] = Seq[GLink]()
    allJson.list().asScala.foreach(r => {
      personsJson +:= GNode("person", r.get("username").asString())
      projectsJson +:= GNode("project", r.get("name").asString())
      linksJson +:= GLink(r.get("type").asString(), r.get("username").asString(), r.get("name").asString())
    })
//    val projectsJson = allJson.list().asScala.map(r => {
//      GNode("project", r.get("name").asString())
//    })
//    val linksJson = allJson.list().asScala.map(r => {
//      GLink(r.get("type").asString())
//    })

    val graph = GGraph(
      personsJson.groupBy(p => p.name).map(g => g._2.head).toSeq ++
        projectsJson.groupBy(p => p.name).map(g => g._2.head).toSeq,
      linksJson)

//    Ok("ok")
    Ok(Json.prettyPrint(Json.toJson(graph))).as(ContentTypes.JSON)
  }
  }

  def refresh() = Action { implicit request: Request[AnyContent] => {

    //    val gitHost = config.underlying.getString("sources.git.host")
    //    val gitToken = config.underlying.getString("sources.git.token")

    val driver = GraphDatabase.driver("bolt://127.0.0.1:7687", AuthTokens.basic("bruno", "bruno"))

    val session = driver.session
    session.run("MATCH (n) DETACH DELETE n")
    val usersJson = getGitUsers
    val pvm = ProjectViewModel(usersJson.map(u => u.name))

    usersJson.foreach(u => session.run(s"""CREATE (:Person {uid: "${u.id}", name: "${u.name}", username: "${u.username}"})"""))

    val projectsJson = getGitProjects()
    projectsJson
      .foreach(p => session.run(
        s"""CREATE (p:Project {uid: "${p.id}", name: "${p.name}", path_with_namespace: "${p.path_with_namespace}"})"""))

    usersJson.foreach(u => {
      val userProjectJson = getGitUserProjects(u.id)
      userProjectJson
        .foreach(p => session.run(
          s"""MATCH (p:Project) WHERE p.uid = "${p.id}"
             |MATCH (u:Person) WHERE u.uid = "${u.id}"
             |CREATE (u)-[:owns]->(p)
          """.stripMargin))
    })

    projectsJson.foreach(p => {
      val projectCommit = getGitProjectCommits(p.id)
      projectCommit
        .filter(u => u.author_email.contains("@"))
        .groupBy(c => c.author_email)
        .foreach(c => {
          val author_name = c._2.head.author_email.split("@")(0)
          println(s"match project ${p.id} with user $author_name")
          val query =
            s"""MATCH (p:Project) WHERE p.uid = "${p.id}"
               |MATCH (u:Person) WHERE u.username = "$author_name"
               |CREATE (u)-[:commit]->(p)
          """.stripMargin
          println(query)
          session.run(query)
        })
    })

    session.close()

    Ok(Json.prettyPrint(Json.toJson(pvm.names))).as(ContentTypes.JSON)
  }
  }

  def getGit(url: String): (String, Option[String]) = {
    val gitHost = config.underlying.getString("sources.git.host")
    val gitToken = config.underlying.getString("sources.git.token")

    val urlPage = if(url.startsWith("http")) url else s"$gitHost/$url"

    println("get url: " + urlPage)
    val request: Future[WSResponse] = ws
      .url(urlPage)
      .withHttpHeaders("Private-Token" -> gitToken)
      .withRequestTimeout(20.seconds)
      .get()
    val response = Await.result(request, 20.seconds)
    val content = new java.lang.String(response.bodyAsBytes.toArray, StandardCharsets.UTF_8)
    val reg = """<(?<link>[^<>]*?)>;\s+rel="next"""".r
    val next = response.headerValues("Link")
    println(next)
    var nextLink: Option[String] = Option.empty[String]
    next.filter(h => h.contains("""rel="next"""))
      .foreach(h => {
        println("contains rel next")
        reg.findFirstMatchIn(h) match {
          case success: Option[Regex.Match] => {
            println("success")
            nextLink = Some(success.get.group("link"))
            println("link")
            println(success.get.group("link"))
          }
        }
      })
    println("next link")
    println(nextLink)
    return (content.toString, nextLink)
  }

  def getGitList(url: String, cachePath: Option[String] = Some(".")): Seq[String] = {
    val db = new DataService()
    val cache = db.getCache(url, cachePath.get)
    if(cache.nonEmpty) {
      return cache.get.split("@@@@@")
    }

    var ret: Seq[String] = Seq[String]()
    var pageUrl = url
    if (pageUrl.contains("?")) {
      pageUrl += "&per_page=100"
    } else {
      pageUrl += "?per_page=100"
    }
    var next: Option[String] = Option.empty[String]
    do {
      var (ret1, next1) = getGit(pageUrl)
      ret +:= ret1
      next = next1
      if(next.nonEmpty) {
        pageUrl = next.get
        println("pageUrl next")
        println(pageUrl)
      }
    } while(next.nonEmpty)
    db.writeCache(url, ret.mkString("@@@@@"), cachePath.get)
    ret
  }

  def getGitUsers: Seq[ResponseUser] = {
    var items = Seq[ResponseUser]()
    getGitList("api/v4/users?active=true").flatMap(l => {
      val json = Json.parse(l).validate[Seq[ResponseUser]]
      val ret: Option[Seq[ResponseUser]] = Option.empty[Seq[ResponseUser]]
      if (json.isSuccess) {
        items ++= json.get
      }
      ret
    })
    items
  }

  def getGitUserProjects(userId: Int): Seq[ResponseUserProject] = {
    var items = Seq[ResponseUserProject]()
    getGitList(s"api/v4/users/$userId/projects").flatMap(l => {
      val json = Json.parse(l).validate[Seq[ResponseUserProject]]
      val ret: Option[Seq[ResponseUserProject]] = Option.empty[Seq[ResponseUserProject]]
      if (json.isSuccess) {
        items ++= json.get
      }
      ret
    })
    items
  }

  def getGitProjects(): Seq[ResponseProject] = {
    var items = Seq[ResponseProject]()
    getGitList(s"api/v4/projects?active=true").flatMap(l => {
      val json = Json.parse(l).validate[Seq[ResponseProject]]
      val ret: Option[Seq[ResponseProject]] = Option.empty[Seq[ResponseProject]]
      if (json.isSuccess) {
        items ++= json.get
      }
      ret
    })
    items
  }

  def getGitProjectCommits(id: Int): Seq[ResponseProjectCommit] = {
    var items = Seq[ResponseProjectCommit]()
    getGitList(s"api/v4/projects/$id/repository/commits", Some("projectcommits")).flatMap(l => {
      val json = Json.parse(l).validate[Seq[ResponseProjectCommit]]
      val ret: Option[Seq[ResponseProjectCommit]] = Option.empty[Seq[ResponseProjectCommit]]
      if (json.isSuccess) {
        items ++= json.get
      }
      ret
    })
    items
  }
}

case class ProjectViewModel(var names: Seq[String])

case class ResponseUser(id: Int, name: String, username: String)

case class ResponseProject(id: Int, name: String, path_with_namespace: String)

case class ResponseUserProject(id: Int, name: String)

case class ResponseProjectCommit(id: String, author_name: String, author_email: String)

case class GGraph(nodes: Seq[GNode], links: Seq[GLink])
case class GNode(label: String, name: String)
case class GLink(label: String, from: String, to: String)