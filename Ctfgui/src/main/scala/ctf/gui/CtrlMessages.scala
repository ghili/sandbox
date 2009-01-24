
package ctf.gui

import ctf.gui.domain._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._


sealed abstract case class CriteriaMessage
case class FichierParDossierCriteria(idDossier:Long) extends CriteriaMessage
case class DossierParSupportSearchCriteria(idSupport:Long, node:DefaultMutableTreeNode) extends CriteriaMessage
case object SearchAllSupport extends CriteriaMessage
object SizeUnitEnumeration extends Enumeration {
    val ko = Value("ko")
    val mo = Value("mo")
    val go = Value("go")
}
object SizeCriteriaEnumeration extends Enumeration {
    val mini = Value("mini")
    val maxi = Value("maxi")
}
case class SearchOption(number:Double,sizeUnit:SizeUnitEnumeration.Value, sizeCriteria:SizeCriteriaEnumeration.Value)
case class FinderCriteria(var value:String,val options:List[SearchOption]) extends CriteriaMessage

sealed abstract case class ResultListMessage{
    val results:List[_]
    val source:Any
}
case class SupportResult(val results:List[Support], val source:Any) extends ResultListMessage
case class FichierResult(val results:List[Fichier], val source:Any) extends ResultListMessage
case class DossierResult(val results:List[Dossier], val source:Any) extends ResultListMessage
