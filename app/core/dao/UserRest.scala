package core.dao

import play.api.mvc.{Action, Controller}
import models.User
import play.api.{Logger, Application}
import scala.Option

/**
 * Created with IntelliJ IDEA.
 * User: bangnv
 * Date: 7/15/13
 * Time: 2:48 PM
 * To change this template use File | Settings | File Templates.
 */
trait UserService {
  def findUserById (userId : String) : Option[User]
  def findUserByUsername (userName : String) : Option[User]
  def findUserByEmail (email : String) : Option[User]
  def findUserByUsernameAndEmail(userName:String, email:String):Option[User]
  def resetPassword(userName:String,oldPassword :String,newPassword :String) : Boolean
  //def getUsers(pageNumber:Int, ItemsByPage:Int, )
  def deleteUserByUserName(userName:String) : Boolean

}

object InMemoryUserService extends UserService {

  private var users = Map[String,User]()
  var user1 = User("1",Option("nguyenvietbang"),"password1",Option("vbang@gmail.com"))
  var user2 = User("2",Option("nguyentuanbao"),"password2",Option("tunbao@gmail.com"))
  var user3 = User("3",Option("nguyentuananh"),"password3",Option("tuananh@gmail.com"))
  users  +=(user1.id -> user1)
  users  +=(user2.id -> user2)
  users  +=(user3.id -> user3)



  //def findUserById(userId: String): Option[User] = ???
  def findUserById(userId: String): Option[User] ={
    if(Logger.isDebugEnabled) {
      Logger.debug("users = %s".format(users))
    }
    users.get(userId)
  }

  def findUserByUsername(username: String): Option[User] ={
    if(Logger.isDebugEnabled) {
      Logger.debug("users = %s".format(users))
    }
    users.values.find( u => u.username.map( e => e == username ).getOrElse(false))
  }

  def findUserByEmail(email: String): Option[User] = {
    if(Logger.isDebugEnabled) {
      Logger.debug("users = %s".format(users))
    }
    users.values.find(u => u.email.map( e => e == email ).getOrElse(false))
  }

  def findUserByUsernameAndEmail(userName: String, email: String): Option[User]  = ???

  def resetPassword(userName: String, oldPassword: String, newPassword: String): Boolean = ???

  def changePassword(username:String,password:String) ={
      var user=findUserByUsername(username).get
      user.password=password
  }

  //def getUsers(pageNumber:Int, ItemsByPage:Int, )
  def deleteUserByUserName(userName: String): Boolean = ???


}
