package APIs

import org.specs2.mutable.Specification
import play.api.test.Helpers._
import play.api.test.{WithApplication, FakeRequest}
import play.api.libs.json.Json
import play.api.test.FakeHeaders
import play.api.mvc.Result
import scala.Some


/**
 * Created with IntelliJ IDEA.
 * User: bangnv
 * Date: 7/17/13
 * Time: 2:35 PM
 * To change this template use File | Settings | File Templates.
 */
class UserRestServiceSpec extends Specification {
  private val USERS_URL = "/users"
  private val CHANGE_PASSWORD_URL = ""
  private val USER_PREFIX_CONTENT_LOCATION = "https://aox.com/users/"

  def sendJsonToServer(url: String, data: Map[String, String], method: String): Option[Result] = {
    route(FakeRequest(method,
      url,
      FakeHeaders(Seq("Content-Type" -> Seq("application/json"))),
      Json.toJson(data)))
  }

  "1 - Create a new  users" should {


    "HTTP/1.1 201 Created" in new WithApplication {
      val Some(result) = sendJsonToServer(USERS_URL, Map("username" -> "joe", "email" -> "joe@example.com"), POST)
      val jsonResponse = Json.parse(contentAsString(result))
      status(result) must equalTo(CREATED)
      contentType(result) must beSome("application/json")
      charset(result) must beSome("utf-8")
      (jsonResponse \ "username").as[String] must equalTo("joe")
      (jsonResponse \ "email").as[String] must equalTo("joe@example.com")
      header(CONTENT_LOCATION, result) must beSome(USER_PREFIX_CONTENT_LOCATION.concat("joe"))

    }

    "HTTP/1.1 409 Conflict " in new WithApplication {
      val Some(result) = sendJsonToServer(USERS_URL, Map("username" -> "nguyenvietbang", "email" -> "joe@example.com"), POST)
      status(result) must equalTo(CONFLICT)
    }


    "HTTP/1.1 410 Gone " in new WithApplication {
      val Some(result) = sendJsonToServer(USERS_URL, Map("username" -> "test", "email" -> "vbang@gmail.com"), POST)
      status(result) must equalTo(GONE)
    }

    "HTTP/1.1 403 Forbidden" in new WithApplication {
      // usename is null
      val Some(result) = sendJsonToServer(USERS_URL, Map("username" -> "", "email" -> "vbang@gmail.com"), POST)
      status(result) must equalTo(FORBIDDEN)
      var Some(result1) = sendJsonToServer(USERS_URL, Map("email" -> "vbang@gmail.com"), POST)
      status(result1) must equalTo(NOT_FOUND)
      // the length of usename is >128
      var username = ""
      1.to(20).foreach(_ => username += "conghoaxahoichunghia")
      var Some(result2) = sendJsonToServer(USERS_URL, Map("username" -> username, "email" -> "vbang@gmail.com"), POST)
      status(result2) must equalTo(FORBIDDEN)

      // the usename can only contain: 0-9 a-z A-Z '.' '-' '_'
      var Some(result3) = sendJsonToServer(USERS_URL, Map("username" -> "safafafs*", "email" -> "vbang@gmail.com"), POST)
      status(result3) must equalTo(FORBIDDEN)
    }

  }



  "2 - Change a new password " should {

    "Status Code 409 - HTTP/1.1 409 Conflict" in new WithApplication {
      val username = "nguyenvietbang"
      val url = USERS_URL.concat("/").concat(username).concat("/password")
      val Some(result) = sendJsonToServer(url, Map("old_password" -> "password1", "new_password" -> "passNotValide"), PUT)
      status(result) must equalTo(CONFLICT)
    }

    "Status Code 404 - HTTP/1.1 404 Not Found - No user bound to the username provided." in new WithApplication {
      val username = "username_non_existent"
      val url = USERS_URL.concat("/").concat(username).concat("/password")
      val Some(result) = sendJsonToServer(url, Map("old_password" -> "password1", "new_password" -> "Conghoaxahoi8"), PUT)
      status(result) must equalTo(NOT_FOUND)
    }

    "Status Code 406 - HTTP/1.1 406 Not Acceptable- The old password does not match." in new WithApplication {
      val username = "nguyenvietbang"
      val url = USERS_URL.concat("/").concat(username).concat("/password")
      val Some(result) = sendJsonToServer(url, Map("old_password" -> "passwordNotMatch", "new_password" -> "Conghoaxahoi8"), PUT)
      status(result) must equalTo(NOT_ACCEPTABLE)
    }

    "Status Code 204 - HTTP/1.1 204 No Content- Password has been changed successfully.." in new WithApplication {
      val username = "nguyenvietbang"
      val url = USERS_URL.concat("/").concat(username).concat("/password")
      val Some(result) = sendJsonToServer(url, Map("old_password" -> "password1", "new_password" -> "Conghoaxahoi8"), PUT)
      status(result) must equalTo(NO_CONTENT)
    }
  }


  "3 - Reset password" should {
    "Status Code 204 - HTTP/1.1 204 No Content" in new WithApplication {
      val username = "nguyenvietbang"
      val url = USERS_URL.concat("/").concat(username).concat("/password_reset")
      val Some(result) = sendJsonToServer(url, Map("email" -> "vbang@gmail.com"), POST)
      status(result) must equalTo(NO_CONTENT)
    }

    "HTTP/1.1 404 Not Found - No user bound to the username provided." in new WithApplication {
      val username = "noexisteUname"
      val url = USERS_URL.concat("/").concat(username).concat("/password_reset")
      val Some(result) = sendJsonToServer(url, Map("email" -> "error@gmail.com"), POST)
      status(result) must equalTo(NOT_FOUND)
    }

    "HTTP/1.1 409 Conflict - The provided email address does not match username" in new WithApplication {
      val username = "nguyenvietbang"
      val url = USERS_URL.concat("/").concat(username).concat("/password_reset")
      val Some(result) = sendJsonToServer(url, Map("email" -> "error@gmail.com"), POST)
      status(result) must equalTo(CONFLICT)
    }
  }


  "4 - get USER" should {

    "HTTP/1.1 200 OK " in new WithApplication {
      val username = "nguyenvietbang"
      val url = USERS_URL.concat("/").concat(username)
      val Some(result) = sendJsonToServer(url, Map.empty[String, String], GET)
      status(result) must equalTo(OK)
      val jsonResponse = Json.parse(contentAsString(result))
      //    println(jsonResponse)
      (jsonResponse \ "username").as[String] must equalTo("nguyenvietbang")
      (jsonResponse \ "email").as[String] must not beEmpty
      var updated = (jsonResponse \ "updated").as[String]
      updated must not beEmpty
      var created = (jsonResponse \ "created").as[String]
      created must not beEmpty
    }
  }


  "6 - set USER" should {
    "HTTP/1.1 200 OK " in new WithApplication {
      val username = "nguyenvietbang"
      val url = USERS_URL.concat("/").concat(username).concat("/put")
      val Some(result) = sendJsonToServer(url, Map("old_password" -> "password1", "new_password" -> "Conghoaxahoi8"), PUT)
      //  status(result) must equalTo(NO_CONTENT)


    }
  }


}
