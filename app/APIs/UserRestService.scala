package APIs

import models.User
import play.api.mvc.{Action, Controller}
import play.api.libs.json.Json.toJson
import scala._
import core.dao.InMemoryUserService
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

  private val OK_MSG = "OK"
  private val NOT_OK_MSG = "KO"
  private val MISSING_PAR_MSG = "Missing parameter [%s]"

  private def errorResponse(msg: String) = BadRequest(toJson(Map(STATUS_KEY -> NOT_OK_MSG, MESSAGE_KEY -> msg)))

  private def missingParameterResponse(parameter: String) = errorResponse(MISSING_PAR_MSG.format(parameter))

  private def userToJson(user: User) = toJson(userToMap(user))

  private def userToMap(user: User) = Map(
    USERNAME_KEY -> user.username.get.toString(),
    OBJECT_ID_KEY -> user.id.toString(),
    CREATED_AT_KEY -> user.created.toString,
    UPDATED_AT_KEY -> user.updated.get.toString(),
    EMAIL_KEY -> user.email.get.toString()
  )


  //case class EmailPreviouslyExistentException(email:String ) extends Exception
  def isValid(email: String): Boolean =
    if ( """(?=[^\s]+)(?=(\w+)@([\w\.]+))""".r.findFirstIn(email) == None) false else true

  def signUp = Action(parse.json) {
    implicit request =>
      println(request.body.getClass)
      var username = (request.body \ USERNAME_KEY)
      var email = (request.body \ EMAIL_KEY)
      def requisitionToNewUser = (username.asOpt[String], email.asOpt[String]) match {
        case (Some(u), _) if (u.trim.isEmpty) | (u.trim.length() > 128 | (!u.matches("[a-zA-Z0-9-_.]*"))) => Left(Forbidden("The username is not valid"))
        case (None, _) => Left(NotAcceptable("The username must not empty"))
        case (_, Some(e)) if (!e.trim.isEmpty && !isValid(e)) =>
          Left(NotAcceptable("The provided email is not valid"))
        case (Some(_), None) => Left(NotAcceptable("The email must not empty"))
        case (_, Some(e)) if (e.trim.isEmpty) => Left(NotAcceptable("The email must not empty"))
        case (Some(u), Some(e)) => Right(User(username = Option(u), email = Option(e)))

      }

      requisitionToNewUser match {
        case Right(user) =>
          User.include(user) match {
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


  /**
   * Dummy method just to test JSON.
   */
  def sayHello = Action(parse.json) {
    request =>
      (request.body \ "name").asOpt[String].map {
        name =>
          Ok(toJson(Map("status" -> "OK", "message" -> ("Hello " + name))))
      }.getOrElse {
        missingParameterResponse("name")
      }
  }


}
