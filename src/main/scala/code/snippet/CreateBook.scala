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


class CreateBook extends Logger {

  
  object title extends RequestVar(Books.create)

  def render ={
    
    /*
    def process() ={
      
      
      def validateField(errorFieldId: String, field: String, text: String) ={
        if(text.length < 2){
          <p class="error" id={errorFieldId}>{field} is too short</p>
        } else {
          <p class="success" id={errorFieldId}>Valid</p>
        }
      }


      def ajaxLiveText(value: String, func: String => JsCmd, attrs: (String, String)*): Elem = {
        S.fmapFunc(S.SFuncHolder(func)) {funcName =>
          (attrs.foldLeft(<input id="title"
                type="text"
                value=""
                onfocus="if (this.value == 'Enter part number') {this.value = '';}"
                onblur="if (this.value == '') {this.value = 'Enter part number';}"
                          />)(_ % _)) %
          ("onkeyup" -> SHtml.makeAjaxCall(
              JsRaw("'" +
                    funcName +
                    "=' + " +
                    "encodeURIComponent(this.value)")
            )
          )
        }
      }
      
      
    }
    */

    //"name=title" #> ajaxLiveText("", x => Replace("title_error", validateField("title_error", "Book Title", x))) andThen
    "name=title" #> SHtml.onSubmit(title.is.title.set(_)) 

  }
  
}