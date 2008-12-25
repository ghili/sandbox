package ctf.gui.ui

import ctf.gui.domain._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._
import ctf.gui.CtrlMessages._

class CtfView(controller:UIController) extends Recherche {

    //listeners initialization

    getInputTextField.addKeyListener(new SearchFieldInputKeyListener(this,controller))
    
    getSupportTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
    val supportTreeEventListener = new SupportTreeEventListener(this,controller)
    getSupportTree.addTreeSelectionListener(supportTreeEventListener)
    getSupportTree.addTreeWillExpandListener(supportTreeEventListener)

    //content initialization

    controller.searchCoordinator ! SearchAllSupport()
}

    
case class DisplayItem
case class FichierDisplayItem(fichier:Fichier) extends DisplayItem { override def toString = fichier.nom}
case class SupportDisplayItem(support:Support) extends DisplayItem { override def toString = support.nom}
case class DossierDisplayItem(dossier:Dossier) extends DisplayItem { override def toString = dossier.nom}