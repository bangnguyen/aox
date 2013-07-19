package APIs

import org.specs2.mutable.Specification

import play.api.libs.json.Json
import play.api.test._
import play.api.test.Helpers._

/**
 * Created with IntelliJ IDEA.
 * User: bangnv
 * Date: 7/16/13
 * Time: 3:11 PM
 * To change this template use File | Settings | File Templates.
 */
class SayHelloSpec extends Specification {

  running(FakeApplication()) {

    "Users.SayHello" should {

      def sendJson(jsonMap: Map[String, String], shouldBeCorrect: Boolean) = {
        running(new FakeApplication) {
          val jsonRequisition = Json.toJson(jsonMap)
          val Some(result) = route(FakeRequest(POST,
            "/hello",
            FakeHeaders(Seq("Content-Type" -> Seq("application/json"))),
            jsonRequisition))

          contentType(result) must beSome("application/json")
          charset(result) must beSome("utf-8")
          val jsonResponse = Json.parse(contentAsString(result))
          if (shouldBeCorrect) {
            status(result) must equalTo(OK)
            (jsonResponse \ "status").as[String] must equalTo("OK")
            val expectedMessage = "Hello " + (jsonRequisition \ "name").as[String]
            (jsonResponse \ "message").as[String] must equalTo(expectedMessage)
          } else {
            status(result) must equalTo(BAD_REQUEST)
            (jsonResponse \ "status").as[String] must equalTo("KO")
            (jsonResponse \ "message").as[String] must equalTo("Missing parameter [name]")
          }
        }
      }

      "Not process a empty String" in {
        sendJson(Map.empty[String, String], false)
      }

      "Not process a JSON with no 'name' parameter" in {
        sendJson(Map("test" -> "foo"), false)
      }

      "Process a JSON with a valid 'name' parameter" in {
        sendJson(Map("name" -> "foo"), true)
      }

      "Process a JSON with a empty 'name' parameter" in {
        sendJson(Map("name" -> ""), true)
      }
    }
  }

}