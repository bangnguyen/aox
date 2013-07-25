package APIs

import models.User
import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json.toJson
import scala._
import core.dao.InMemoryUserService
import scala.Predef._
import models.User.UsernamePreviouslyExistentException
import scala.Some
import models.User.EmailPreviouslyExistentException


/**
 * Created with IntelliJ IDEA.
 * User: bangnv
 * Date: 7/16/13
 * Time: 10:52 AM
 * To change this template use File | Settings | File Templates.
 */
object UserRestService extends Controller {
  private val USER_PREFIX_CONTENT_LOCATION = "https://aox.com/users/"
  private val OBJECT_ID_KEY = "id"
  private val USERNAME_KEY = "username"
  private val PASSWORD_KEY = "password"
  private val OLD_PASSWORD_KEY = "old_password"
  private val NEW_PASSWORD_KEY = "new_password"
  private val EMAIL_KEY = "email"
  private val CREATED_AT_KEY = "created"
  private val UPDATED_AT_KEY = "updated"
  private val MESSAGE_KEY = "message"
  private val STATUS_KEY = "status"
  private val MISSING_PARAMS_MSG = "Required parameter is missing"
  private val REQUIRE_KEY = "require"
  private val LINKS_KEY = "_links"
  private val HREF_KEY = "href"
  private val SELF_KEY = "self"

  private val OK_MSG = "OK"
  private val NOT_OK_MSG = "KO"


  private def errorResponse(msg: String) = BadRequest(toJson(Map(STATUS_KEY -> NOT_OK_MSG, MESSAGE_KEY -> msg)))


  private def userToJson(user: User) = toJson(userToMap(user))


  private def userToMap(user: User) = Map(

    LINKS_KEY -> toJson(Map(SELF_KEY -> toJson(Map(HREF_KEY -> "/users/".concat(user.username.get.toString))))).toString(),
    OBJECT_ID_KEY -> user.id.toString(),
    CREATED_AT_KEY -> user.created.get.toString,
    UPDATED_AT_KEY -> user.updated.get.toString(),
    EMAIL_KEY -> user.email.get.toString(),
    USERNAME_KEY -> user.username.get.toString()
  )


  //case class EmailPreviouslyExistentException(email:String ) extends Exception
  def isValid(email: String): Boolean =
    if ( """(?=[^\s]+)(?=(\w+)@([\w\.]+))""".r.findFirstIn(email) == None) false else true

  def signUp = Action(parse.json) {
    implicit request =>
    //  println(request.body.getClass)
      var username = (request.body \ USERNAME_KEY)
      var email = (request.body \ EMAIL_KEY)
      def requisitionToNewUser = (username.asOpt[String], email.asOpt[String]) match {
        case (None, _) => Left(NotFound(toJson(Map(MESSAGE_KEY -> MISSING_PARAMS_MSG, REQUIRE_KEY -> List("username").toString()))))
        case (_, None) => Left(NotFound(toJson(Map(MESSAGE_KEY -> MISSING_PARAMS_MSG, REQUIRE_KEY -> List("email").toString()))))
        case (None, None) => Left(NotFound(toJson(Map(MESSAGE_KEY -> MISSING_PARAMS_MSG, REQUIRE_KEY -> List("username", "email").toString()))))
        case (Some(u), _) if (u.trim.isEmpty) | (u.trim.length() > 128 | (!u.matches("[a-zA-Z0-9-_.]*"))) => Left(Forbidden("The username is not valid"))
        case (_, Some(e)) if (!e.trim.isEmpty && !isValid(e)) =>
          Left(NotAcceptable("The provided email is not valid"))
        case (Some(_), None) => Left(NotAcceptable("The email must not empty"))
        case (_, Some(e)) if (e.trim.isEmpty) => Left(NotAcceptable("The email must not empty"))
        case (Some(u), Some(e)) => Right(User(username = Option(u), email = Option(e)))

      }

      requisitionToNewUser match {
        case Right(user) =>
          InMemoryUserService.findByUsernameOrEmail(user) match {
            case Right(newUser) => {
              Created(userToJson(newUser)).withHeaders(CONTENT_LOCATION -> USER_PREFIX_CONTENT_LOCATION.concat(username.asOpt[String].get))
            }
            case Left(UsernamePreviouslyExistentException(username)) => {
              Conflict(" The provided username" + username + " has been used by someone else.")
            }
            case Left(EmailPreviouslyExistentException(email)) => {
              Gone("The provided email " + email + " has been used by another user.")
            }
          }
        case Left(error) => error

      }

  }

  def changePassword(username: String) = Action(parse.json) {
    implicit request =>

      val old_password = (request.body \ OLD_PASSWORD_KEY)
      val new_password = (request.body \ NEW_PASSWORD_KEY)

      def requisitionParameters = (old_password.asOpt[String], new_password.asOpt[String]) match {
        case (_, Some(n)) if (!n.matches("(?=.*?[0-9])(?=.*?[A-Z]).{8,}")) => {
          Left(Conflict("The new password is not valid"))
        }
        case (Some(o), Some(n)) if (!o.trim.isEmpty && !n.trim.isEmpty) => {
          Right(1)
        }
      }
      requisitionParameters match {
        case Right(1) => {
          val user = InMemoryUserService.findUserByUsername(username)
          if (user.equals(None)) NotFound("No user bound to the " + username + " provided.")
          else if (!user.get.password.equals(old_password.asOpt[String].get)) NotAcceptable("The old password does not match")
          else {
            InMemoryUserService.changePassword(username, new_password.toString())
            NoContent
          }
        }
        case Left(error) => error
      }


  }


  def resetPassword(username: String) = Action(parse.json) {
    implicit request =>
      val email = (request.body \ EMAIL_KEY)
      def emailOpt = (email.asOpt[String])
      // println(emailOpt)
      emailOpt match {
        case None => NotFound(toJson(Map(MESSAGE_KEY -> MISSING_PARAMS_MSG, REQUIRE_KEY -> List("email").toString())))
        case (Some(e)) if (!isValid(e)) => NotAcceptable("The provided email is not valid")
        case (Some(_)) => {
          val user = InMemoryUserService.findUserByUsername(username)
          user match {
            case None => NotFound("No user bound to the " + username + " provided.")
            case (Some(u)) if (!u.email.equals(emailOpt)) => Conflict("No user bound to the " + username + " provided.")
            case (_) => {
              // send email here
              NoContent
            }
          }
        }

      }
  }


  def list(username: String) = Action(parse.json) {
    implicit request =>
      val user = InMemoryUserService.findUserByUsername(username)
      user match {
        case None => NotFound("No user bound to the username" + username + " provided.")
        case Some(u) => {
          Ok(userToJson(u))
        }

      }
  }


  def put(username: String) = Action(parse.json) {
    implicit request =>
      val user = InMemoryUserService.findUserByUsername(username)
      user match {
        case None => NotFound("No user bound to the username " + username + " provided.")
        case Some(e) => {

        }
      }
     Ok("lala")
  }


}
