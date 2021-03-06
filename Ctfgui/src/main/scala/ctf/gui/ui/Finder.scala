
package ctf.gui.ui

import ctf.gui.domain._
import javax.swing._

class FinderView(env: {val view:CtfView}) {

    def loadResultFileList(fichiers:List[Fichier]){
        env.view.searchResultTable model_= new SearchResultTableModel(fichiers)
    }
}

import scala.util.parsing.combinator.syntactical._
import ctf.gui._

object FinderCriteriaBuilder  extends StandardTokenParsers {
    lexical.reserved += ("trouver", "de", "ko", "Mo", "Go", "mini", "maxi")

    val sizeUnit = ("ko"|"Mo"|"Go") ^^ {
        case "ko" => SizeUnitEnumeration.ko
        case "Mo" => SizeUnitEnumeration.mo
        case "Go" => SizeUnitEnumeration.go
    }

    val sizeCriteria = ("mini"|"maxi") ^^ {
        case "mini" => SizeCriteriaEnumeration.mini
        case "maxi" => SizeCriteriaEnumeration.maxi
    }

    val searchOption = "de" ~> numericLit ~ sizeUnit ~ sizeCriteria ^^ { case n ~ u ~ c => SearchOption(n.toDouble,u,c)}

    val searchOptions = rep(searchOption)

    val searchExpression = "trouver" ~> stringLit ~ searchOptions ^^ {case s ~ o => FinderCriteria(s,o)}

    val simpleCriteria = ident ^^ { case s => FinderCriteria(s,Nil)}

    val searchCriteria =   searchExpression  | simpleCriteria

    def parse(s:String, p:Parser[Any]) = {
        p(new lexical.Scanner(s)) match {
            case Success(obj,_) => obj
            case x @ _ => throw new Exception("parsing failed:" + x)
        }
    }

    def parseWithDefaultValue(s:String) = {
        searchCriteria(new lexical.Scanner(s)) match {
            case Success(obj,_) => obj
            case _ => FinderCriteria(s,Nil)
        }
    }
}

