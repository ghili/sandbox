
package ctf.gui.ui

import ctf.gui.domain._
import collection.immutable
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._

class BrowserAction(view:CtfView) {


    /**
     *
     */
    def loadFileList(fichiers:List[Fichier]){
        val rlist = new DefaultListModel()
        for(fichier <- fichiers){
            rlist addElement FichierDisplayItem(fichier)
        }
        view.getFileList.setModel(rlist)
    }

    /**
     *
     */
    def loadSupportTree(supports:List[Support]) = {
        val rootNode = new DefaultMutableTreeNode("supports");
        for(support <- supports){
            val node = new DefaultMutableTreeNode(SupportDisplayItem(support))
            rootNode add node
        }
        view.getSupportTree.setModel(new DefaultTreeModel(rootNode))
    }

    /**
     *
     */
    def addDossierToNode(dossiers:List[Dossier], rootNode:DefaultMutableTreeNode):Long = {
        val dossierRacine = dossiers.find {dossier:Dossier => dossier.dossierParent == null}
        .getOrElse(throw new Exception("dossier racine introuvable"))

        rootNode.getUserObject match {
            case suportDisplayItem:SupportDisplayItem => rootNode.setUserObject(DossierRacineDisplayItem(dossierRacine,suportDisplayItem.support))
            case _ => 
        }

        var nodes = immutable.Map[Long, DefaultMutableTreeNode](dossierRacine.idDossier -> rootNode)
        for(dossier <- dossiers if (dossier.dossierParent != null)){
            val parentNode = nodes.get(dossier.dossierParent.idDossier)
            .getOrElse(throw new Exception("dossier parent introuvable " + dossier.dossierParent.idDossier))
            
            val node = new DefaultMutableTreeNode(DossierDisplayItem(dossier))
            parentNode add node
            nodes += dossier.idDossier -> node
        }
        dossierRacine.idDossier
    }
}
