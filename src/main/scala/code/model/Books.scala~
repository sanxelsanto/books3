package code
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.http._
import scala.xml.{NodeSeq,Text}


object Books extends Books with LongKeyedMetaMapper[Books] with Logger{  

  override def dbTableName = "recipes" // define the DB table name
  override def fieldOrder = List(id, title, description, userid, timecreated, published)

}


class Books extends LongKeyedMapper[Books] with IdPK {

  def getSingleton = Books // what's the "meta" server

  object title extends MappedString(this, 200){
    override val fieldId = Some(Text("titleId"))
    override def validations = 
      valMinLen(3, S.?("title.too.short")) _ ::
      valUnique(S.?("title.already.taken")) _ ::
      super.validations
      /*
      def validate_title( in : String) = {
         if( in.length < 3 ){
            List(FieldError(this, "title too short"))
         }
         else{
            List[FieldError]()
         }
      }

      override def validations = validate_title _ :: Nil
      */

  }
  object description extends MappedString(this, 200){
    override val fieldId = Some(Text("descriptionId"))
    override def validations = 
      valMinLen(3, S.?("description.too.short")) _ ::
      super.validations
  }
  object userid extends MappedLongForeignKey(this, User)
  object timecreated extends MappedLong(this)
  object published extends MappedBoolean(this)

}

