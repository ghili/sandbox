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

class CtfView(env: {val controller:UIController}) extends Recherche {

    //listeners initialization

    val searchButton = new Button{
        override lazy val peer = getSearchJButton
        reactions += {
            case ButtonClicked(_) =>
                env.controller.searchCoordinator ! FinderCriteriaBuilder.parseWithDefaultValue(getSearchJTextField().getText)
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
    val supportTreeEventListener = new SupportTreeEventListener(this,env.controller)
    getSupportTree.addTreeSelectionListener(supportTreeEventListener)
    getSupportTree.addTreeWillExpandListener(supportTreeEventListener)

    //content initialization

    initSupport

    def initSupport = env.controller.searchCoordinator ! SearchAllSupport
}


sealed abstract case class DisplayItem
//case class FichierDisplayItem(fichier:Fichier) extends DisplayItem { override def toString = fichier.nom + fichier.extension }

case class SupportDisplayItem(support:Support) extends DisplayItem { override def toString = support.nom}
case class DossierDisplayItem(dossier:Dossier) extends DisplayItem { override def toString = dossier.nom}
case class DossierRacineDisplayItem(override val dossier:Dossier, support:Support) extends DossierDisplayItem(dossier) {
    override def toString = support.nom + dossier.nom
}

trait FichierTableModel extends AbstractTableModel {
    var columns:List[String]=List()
    val fichiers:List[Fichier]
    type ColumnFunction = PartialFunction[Int, (Fichier => Object)]
    var columnValues:List[ColumnFunction]=List()

    override def getColumnCount = columns.size

    override def getColumnName(columnIndex:Int) = columns(columnIndex)

    override def getRowCount = fichiers.size

    override def getValueAt(rowIndex:Int, columnIndex:Int) = {
        (columnValues.find {(_:ColumnFunction) isDefinedAt columnIndex }
        .getOrElse(throw new Exception("no function for column"+columnIndex)))(columnIndex)(fichiers(rowIndex))
    }

    protected def getSizeString(fichier:Fichier):String = {
        if (fichier.taille>1000000000){ (fichier.taille/1000000000) + " Go"}
        else if (fichier.taille>1000000){ (fichier.taille/1000000) + " Mo"}
        else if (fichier.taille>1000){ (fichier.taille/1000) + " ko"}
        else {
            fichier.taille +" bytes"
        }
    }
}

class BrowserTableModel(val fichiers:List[Fichier]) extends FichierTableModel{
    columns ++= List("file name", "size")
    columnValues += {
        case 0 => {fichier:Fichier => fichier.nom + fichier.extension}
        case 1 => getSizeString(_:Fichier)
    }
}

class SearchResultTableModel(override val fichiers:List[Fichier]) extends BrowserTableModel(fichiers){
    columns ++= List("support", "path")
    columnValues += {
        case 2 => (_:Fichier).dossier.support.nom
        case 3 => (_:Fichier).dossier.chemin
    }
}