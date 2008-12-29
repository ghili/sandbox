
package ctf.gui

import ctf.gui.domain._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._


sealed abstract case class CriteriaMessage
case class FichierParDossierCriteria(idDossier:Long) extends CriteriaMessage
case class DossierParSupportSearchCriteria(idSupport:Long, node:DefaultMutableTreeNode) extends CriteriaMessage
case class SearchAllSupport extends CriteriaMessage
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

sealed abstract case class ResultListMessage(results:List[_], source:Any)
case class SupportResult(supports:List[Support], override val source:Any) extends ResultListMessage(supports, source)
case class FichierResult(fichiers:List[Fichier], override val source:Any) extends ResultListMessage(fichiers, source)
case class DossierResult(dossiers:List[Dossier], override val source:Any) extends ResultListMessage(dossiers, source)
