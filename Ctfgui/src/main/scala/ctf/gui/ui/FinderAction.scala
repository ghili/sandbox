
package ctf.gui.ui

import ctf.gui.domain._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._

class FinderAction(view:CtfView) {

    def loadResultFileList(fichiers:List[Fichier]){
        val rlist = new DefaultListModel()
        for(fichier <- fichiers){
            rlist addElement FichierDisplayItem(fichier)
        }
        view.getResultList.setModel(rlist)
    }
}
