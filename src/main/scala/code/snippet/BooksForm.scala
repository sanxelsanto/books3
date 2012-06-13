package code
package snippet

import net.liftweb._
import http._
import common._
import util.Helpers._
import scala.xml.{NodeSeq,Text}
import model._
import util._
import net.liftweb.mapper._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js._
import net.liftweb.http.js.JE._
import scala.xml._ // not found: type Elem


class BooksForm extends StatefulSnippet with Loggable{

  private var title = ""
  private var description = "" 
  private val book = Books.create

  def dispatch = {
    case "add" => add _
  }


  def add( xhtml: NodeSeq ): NodeSeq = {

    def doProcess() {
      val book : Books =  Books.create.title(title)
      .description(description)
      .timecreated(Helpers.timeNow.getTime)
      .published(true)
      book.validate match{
        case Nil =>
          S.notice("book is add")
          book.saveMe
          S.redirectTo("/static/index")
        case errors : List[FieldError] =>
          S.error(errors)
      }
    }

    def ajaxLiveText( value: String, func: String => JsCmd, attrs: (String, String)* ): Elem = {
      S.fmapFunc(S.SFuncHolder(func)) { 
        funcName =>
          (attrs.foldLeft(<input />)(_ % _)) %
          ("onkeyup" -> SHtml.makeAjaxCall( JsRaw("'" + funcName + "=' + " + "encodeURIComponent(this.value)") ) )
      }
    }
    

    def check_title(in:String, field: String, errorFieldId: String) = {
      
      val title = in.trim
      val book : Books =  Books.create.title(title)
      
      book.title.validate match{
        case Nil =>
          <p class="success" id={errorFieldId}>Valid</p>
        case errors : List[FieldError] =>
          S.error(errors)
          <p class="error" id={errorFieldId}><lift:Msg id="books_title" /></p>
      }
      
    }        
    
    def check_description(in:String, field: String, errorFieldId: String) = {

      val description = in
      val book : Books =  Books.create.description(description)

      book.description.validate match{
        case Nil =>
          <p class="success" id={errorFieldId}>Valid</p>
        case errors : List[FieldError] =>
          S.error(errors)
          <p class="error" id={errorFieldId}><lift:Msg id="books_description" /></p>
      }

    }
     
    ////////////////////////////////////////////////////////////////////////////////////////////
    
    def cssSel =     
    "name=title"  #> ajaxLiveText( "", title => Replace( "title_error", 
                                                         check_title( title,"Title: ", "title_error" ) ), 
                                                         "type" -> "text"
                                 ) andThen
    "name=title"  #> SHtml.text(title, title = _, "maxlength" -> "40") andThen    
    "name=description"  #> ajaxLiveText( "", title => Replace( "description_error", 
                                                         check_description( description,"Description: ", "description_error" ) ), 
                                                         "type" -> "text"
                                 ) andThen
    "name=description"  #> SHtml.text(description, description = _, "maxlength" -> "40") andThen
    "type=submit" #> SHtml.submit("Send", doProcess)
    cssSel.apply(xhtml)

  }


}