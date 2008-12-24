package ctf.gui.ui

import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._


class SearchFieldInputKeyListener(view:CtfView,controller:UIController) extends KeyAdapter{
    
    override
    def keyPressed(e:KeyEvent) = {
        if (e.getKeyCode == 10){
            controller.searchCoordinator ! FichierSearchCriteria(view.getInputTextField getText)
        }
    }
}
