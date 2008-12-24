package ctf.gui.ui

import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._
import ctf.gui.CtrlMessages._

class SupportTreeEventListener(view:CtfView,controller:UIController) extends TreeSelectionListener with TreeWillExpandListener{

    def valueChanged(event:TreeSelectionEvent)= refreshSupportOnPath(event.getPath)

    def treeWillExpand(event:TreeExpansionEvent)= refreshSupportOnPath(event.getPath)

    def treeWillCollapse(event:TreeExpansionEvent)= {}

    private def refreshSupportOnPath(path:TreePath) = {
        val node = path.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
        node.getUserObject match {
            case SupportDisplayItem(support) => controller.searchCoordinator ! DossierParSupportSearchCriteria(support.idSupport, node)
            case DossierDisplayItem(dossier) => controller.searchCoordinator ! FichierParDossierCriteria(dossier.idDossier)
            case _ =>
        }
    }
}
