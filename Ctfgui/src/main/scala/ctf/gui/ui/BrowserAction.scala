
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
    def addDossierToNode(dossiers:List[Dossier], supportNode:DefaultMutableTreeNode){
        var nodes = immutable.Map[Long, DefaultMutableTreeNode]()
        for(dossier <- dossiers){

            val parentNode= if (dossier.dossierParent == null){
                println("dossierParent est null")
                supportNode
            }else{
                val idDossierParent = dossier.dossierParent.idDossier
                nodes.get(idDossierParent) getOrElse supportNode
            }
            val node = new DefaultMutableTreeNode(DossierDisplayItem(dossier))
            parentNode add node
            nodes += dossier.idDossier -> node
        }
    }
}
