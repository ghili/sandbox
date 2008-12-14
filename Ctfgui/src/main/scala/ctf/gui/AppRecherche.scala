package ctf.gui

import ui.Recherche
import java.awt.event._
import javax.swing._
import java.util.{List => JavaList}
import collection.jcl.BufferWrapper
import domain._
import scala.actors._
import scala.actors.Actor._

object AppRecherche extends Recherche {

    initEventListeners
    val searcher = new Searcher
    
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

    def searchCoordinator:Actor = actor{
        loop{
            react{
                case FileSearchCriteria(name) =>
                    searcher.fileSearcher ! FileSearchCriteria(name)
                case results:List[Fichier] =>
                    println(results.size + " results found")
                    val rlist = new DefaultListModel()
                    for(e <- results){
                        rlist.addElement(e.nom)
                    }
                    getResultList.setModel(rlist)
                    exit('stop)
            }
        }
    }


    
    class InputKeyListener extends KeyAdapter{
        override
        def keyPressed(e:KeyEvent) = {
            if (e.getKeyCode == 10){
                searchCoordinator ! FileSearchCriteria(getInputTextField.getText)
            }
            //println("event="+e)
        }
    }
}
