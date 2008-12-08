package ctf.gui

import ui.Recherche
import java.awt.event._
import javax.swing._
import java.util.{List => JavaList}
import collection.jcl.BufferWrapper
import domain._

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
                val results = new BufferWrapper[Fichier]{
                    def underlying = SqlMapConfig.sqlMapper
                    .queryForList("rechercheFichier", "%"+getInputTextField.getText+"%")
                    .asInstanceOf[JavaList[Fichier]]}.toList

                println(results.size + " results found")
                val rlist = new DefaultListModel()
                for(e <- results){
                    rlist.addElement(e.nom)
                }
                getResultList.setModel(rlist)
            }
            //println("event="+e)
        }
    }
}
