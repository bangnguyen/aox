package models

import core.dao.InMemoryUserService
import java.util.Date

/**
 * Created with IntelliJ IDEA.
 * User: bangnv
 * Date: 7/15/13
 * Time: 2:24 PM
 * To change this template use File | Settings | File Templates.
 */
case class User(
                 id:String=" ",
                 username : Option[String],
                 var password : String=" ",
                 var  email: Option[String],
                 var created: Date = new Date(),
                 var  updated: Option[Date] = None
                 )
object User {

  case class UsernamePreviouslyExistentException(username:String ) extends Exception
  case class EmailPreviouslyExistentException(email:String ) extends Exception



  private def validateUserToBeIncluded(user: User): Option[Throwable] = {
    InMemoryUserService.findUserByUsername(user.username.get) match {
      case Some(_) =>{Some(UsernamePreviouslyExistentException(user.username.get))   }
      case None   => {
        InMemoryUserService.findUserByEmail(user.email.get)  match {
          case Some(_) => {Some(EmailPreviouslyExistentException(user.email.get)) }
          case None => None
        }
      }
    }
  }
  def include(user:User):Either[Throwable,User]={
    this.validateUserToBeIncluded(user) match {
      case Some(e) => Left(e)
      case None => {
        var newUser=user.copy(updated = Some(user.created))
        Right(newUser);
      }
    }
  }

}

