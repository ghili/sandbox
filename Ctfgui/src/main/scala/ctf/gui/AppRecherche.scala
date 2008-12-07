package ctf.gui

import ui.Recherche
import java.awt.event._

object AppRecherche extends Recherche {

    initEventListeners
    
    def initEventListeners(){
        getInputTextField.addKeyListener(new InputKeyListener)
    }

    def main(args : Array[String]) = {
        java.awt.EventQueue.invokeLater(new Runnable() {
                def run() {
                    setVisible(true)
                }
            })
    }


    
    class InputKeyListener extends KeyAdapter{
        override
        def keyPressed(e:KeyEvent) = {
            if (e.getKeyCode == 10){
                println(SqlMapConfig.sqlMapper.queryForList("rechercheFichier", "%"+getInputTextField.getText+"%"))
            }
            //println("event="+e)
        }
    }
}
