package ctf.gui.ui

import ctf.gui.domain._
import scala.swing.Component._
import scala.swing._
import scala.swing.event._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._
import ctf.gui.SearchAllSupport

class CtfView(controller:UIController) extends Recherche {

    //listeners initialization

    val searchButton = new Button{
        override lazy val peer = getSearchJButton
        reactions += {
            case ButtonClicked(_) =>
                controller.searchCoordinator ! FinderCriteriaBuilder.parseWithDefaultValue(getSearchJTextField().getText)
        }
    }

    val refreshButton = new Button{
        override lazy val peer = getRefreshJButton
        reactions += {
            case ButtonClicked(_) => initSupport
        }
    }

    //getRefreshButton.addActionListener()
    
    getSupportTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
    val supportTreeEventListener = new SupportTreeEventListener(this,controller)
    getSupportTree.addTreeSelectionListener(supportTreeEventListener)
    getSupportTree.addTreeWillExpandListener(supportTreeEventListener)

    //content initialization

    initSupport

    def initSupport = controller.searchCoordinator ! SearchAllSupport()
}


case class DisplayItem
case class FichierDisplayItem(fichier:Fichier) extends DisplayItem { override def toString = fichier.nom + fichier.extension }
case class FichierSearchResultDisplayItem(fichier:Fichier) extends DisplayItem {
    override def toString = fichier.nom + fichier.extension + "             (" + fichier.taille +" bytes, "+ fichier.dossier.support.nom+"@/"+fichier.dossier.chemin+")"
}
case class SupportDisplayItem(support:Support) extends DisplayItem { override def toString = support.nom}
case class DossierDisplayItem(dossier:Dossier) extends DisplayItem { override def toString = dossier.nom}
case class DossierRacineDisplayItem(override val dossier:Dossier, support:Support) extends DossierDisplayItem(dossier) {
    override def toString = support.nom + dossier.nom
}