package models

import java.util.Date

/**
 * Created with IntelliJ IDEA.
 * User: bangnv
 * Date: 7/15/13
 * Time: 2:24 PM
 * To change this template use File | Settings | File Templates.
 */
case class User(
                 id: String = " ",
                 username: Option[String],
                 var password: String = " ",
                 var email: Option[String],
                 var created: Option[Date] = Option(new Date()),
                 var updated: Option[Date] = Option(new Date())
                 ) {

  def apply(params: Map[String, String]) = {
    params.foreach {
      case ("email", e) => email = Some(e);
      case ("password", p) => password = p;
    }
    updated = Option(new Date())
  }
}

object User {

  case class UsernamePreviouslyExistentException(username: String) extends Exception

  case class EmailPreviouslyExistentException(email: String) extends Exception

  case class UserNotFoundException(username: String) extends Exception


}

