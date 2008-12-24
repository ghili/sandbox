
package ctf.gui

import ctf.gui.domain._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._


object CtrlMessages {
    case class FichierSearchCriteria(name:String)
    case class FichierParDossierCriteria(idDossier:Long)
    case class DossierParSupportSearchCriteria(idSupport:Long, node:DefaultMutableTreeNode)
    
    case class SearchAllSupport

    case class ResultList(results:List[_], source:Any)
    case class SupportResult(supports:List[Support], override val source:Any) extends ResultList(supports, source)
    case class FichierResult(fichiers:List[Fichier], override val source:Any) extends ResultList(fichiers, source)
    case class DossierResult(dossiers:List[Dossier], override val source:Any) extends ResultList(dossiers, source)

}