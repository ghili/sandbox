
package ctf.gui

import ctf.gui.domain._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._


object CtrlMessages {
    case class FichierSearchCriteria(name:String)
    case class FichierParDossierCriteria(idDossier:Long)
    case class DossierParSupportSearchCriteria(idSupport:Long)
    
    case class SearchAllSupport

    case class SupportResult(supports:List[Support])
    case class FichierResult(fichiers:List[Fichier])
    case class DossierResult(dossiers:List[Dossier])

    case class LoadSupport(idSupport:Long, node:DefaultMutableTreeNode)
}