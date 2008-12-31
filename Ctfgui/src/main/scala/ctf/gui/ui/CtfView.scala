package ctf.gui.ui

import ctf.gui.domain._
import scala.swing.Component._
import scala.swing._
import scala.swing.event._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._
import javax.swing.table._
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

    val searchResultTable = new Table{
        override lazy val peer = getSearchResultJTable
    }

    val browserTable = new Table{
        override lazy val peer = getBrowserJTable
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


sealed abstract case class DisplayItem
//case class FichierDisplayItem(fichier:Fichier) extends DisplayItem { override def toString = fichier.nom + fichier.extension }

case class SupportDisplayItem(support:Support) extends DisplayItem { override def toString = support.nom}
case class DossierDisplayItem(dossier:Dossier) extends DisplayItem { override def toString = dossier.nom}
case class DossierRacineDisplayItem(override val dossier:Dossier, support:Support) extends DossierDisplayItem(dossier) {
    override def toString = support.nom + dossier.nom
}

trait FichierTableModel extends AbstractTableModel {
    val columns:List[String]
    val fichiers:List[Fichier]

    override def getColumnName(columnIndex:Int) = columns(columnIndex)

    override def getRowCount = fichiers.size

    protected def getSizeString(fichier:Fichier):String = {
        if (fichier.taille>1000000000){ (fichier.taille/1000000000) + " Go"}
        else if (fichier.taille>1000000){ (fichier.taille/1000000) + " Mo"}
        else if (fichier.taille>1000){ (fichier.taille/1000) + " ko"}
        else {
            fichier.taille +" bytes"
        }
    }
}

class BrowserTableModel(val fichiers:List[Fichier]) extends AbstractTableModel with FichierTableModel{
    val columns = List("file name", "size")
    
    override def getColumnCount = 2

    override def getValueAt(rowIndex:Int, columnIndex:Int) = {
        val fichier = fichiers(rowIndex)
        columnIndex match{
            case 0 => fichier.nom + fichier.extension
            case 1 => getSizeString(fichier)
            case _ => throw new Exception("unknown column : "+columnIndex)
        }
    }

}

class SearchResultTableModel(val fichiers:List[Fichier]) extends AbstractTableModel with FichierTableModel{
    val columns = List("file name", "size", "support", "path")

    override def getColumnCount = 4

    override def getValueAt(rowIndex:Int, columnIndex:Int) = {
        val fichier = fichiers(rowIndex)
        columnIndex match{
            case 0 => fichier.nom + fichier.extension
            case 1 => getSizeString(fichier)
            case 2 => fichier.dossier.support.nom
            case 3 => fichier.dossier.chemin
            case _ => throw new Exception("unknown column : "+columnIndex)
        }
    }
}