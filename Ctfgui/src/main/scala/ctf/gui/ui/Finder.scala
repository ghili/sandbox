
package ctf.gui.ui

import ctf.gui.domain._
import javax.swing._

class FinderView(view:CtfView) {

    def loadResultFileList(fichiers:List[Fichier]){
        val rlist = new DefaultListModel()
        for(fichier <- fichiers){
            rlist addElement FichierSearchResultDisplayItem(fichier)
        }
        view.getResultList.setModel(rlist)
    }
}

import scala.util.parsing.combinator.syntactical._
import ctf.gui._

object FinderCriteriaBuilder  extends StandardTokenParsers {
    lexical.reserved += ("trouver", "de", "ko", "Mo", "Go", "mini", "maxi")

    def sizeUnit = ("ko"|"Mo"|"Go") ^^ {
        case "ko" => SizeUnitEnumeration.ko
        case "Mo" => SizeUnitEnumeration.mo
        case "Go" => SizeUnitEnumeration.go
    }

    def sizeCriteria = ("mini"|"maxi") ^^ {
        case "mini" => SizeCriteriaEnumeration.mini
        case "maxi" => SizeCriteriaEnumeration.maxi
    }

    def searchOption = "de" ~> numericLit ~ sizeUnit ~ sizeCriteria ^^ { case n ~ u ~ c => SearchOption(n.toDouble,u,c)}

    def searchOptions = rep(searchOption)

    def searchExpression = "trouver" ~> stringLit ~ searchOptions ^^ {case s ~ o => FinderCriteria(s,o)}

    def simpleCriteria = ident ^^ { case s => FinderCriteria(s,Nil)}

    def searchCriteria =   searchExpression  | simpleCriteria

    def parse(s:String, p:Parser[Any]) = {
        val result = p(new lexical.Scanner(s))
        result match {
            case Success(obj,_) => obj
            case _ => throw new Exception("parsing failed:"+result)
        }
    }
}

