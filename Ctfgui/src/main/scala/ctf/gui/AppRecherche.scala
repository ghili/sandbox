package ctf.gui

import actors._
import actors.Actor._
import collection.jcl.BufferWrapper
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._
import java.util.{List => JavaList}
import ui.Recherche
import domain._


object AppRecherche extends Recherche {

    initEventListeners
    init
    val searcher = new Searcher
    
    def initEventListeners = {
        getInputTextField.addKeyListener(new InputKeyListener)
        getSupportTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
        getSupportTree.addTreeSelectionListener(new TreeSelectionListener{
                def valueChanged(e:TreeSelectionEvent){
                    val node = e.getPath.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
                    node.getUserObject match {
                        case SupportDisplayItem(support) => searchCoordinator ! LoadSupport(support.idSupport, node)
                        case DossierDisplayItem(dossier) => searchCoordinator ! FichierParDossierCriteria(dossier.idDossier)
                        case _ => 
                    }
                }
            });
        getSupportTree.addTreeWillExpandListener(new SupportTreeListener)
    }

    def init = {
        searchCoordinator ! SearchAllSupport
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
                case fichierCriteria:FichierSearchCriteria =>
                    searcher.fileSearcher ! fichierCriteria
                case SearchAllSupport =>
                    searcher.basicSearcher ! SearchAllSupport
                case LoadSupport(idSupport,node) =>
                    searcher.basicSearcher ! DossierParSupportSearchCriteria(idSupport)
                    receive {
                        case dossierResult:DossierResult =>
                            println(dossierResult.dossiers.size + " dossiers found")
                            addDossierToNode(dossierResult.dossiers,node)
                    }
                case fichierParDossierCriteria:FichierParDossierCriteria =>
                    searcher.basicSearcher ! fichierParDossierCriteria
                    receive {
                        case fichierResult:FichierResult =>
                            println(fichierResult.fichiers.size + " fichiers found")
                            loadFileList(fichierResult.fichiers)
                    }
                case fichierResult:FichierResult =>
                    println(fichierResult.fichiers.size + " files found")
                    loadResultFileList(fichierResult.fichiers)
                    exit('stop)
                case supportResult:SupportResult =>
                    println(supportResult.supports.size + " supports found")
                    loadSupportTree(supportResult.supports)

            }
        }
    }

    def loadResultFileList(fichiers:List[Fichier]){
        val rlist = new DefaultListModel()
        for(fichier <- fichiers){
            rlist addElement FichierDisplayItem(fichier)
        }
        getResultList.setModel(rlist)
    }

    def loadFileList(fichiers:List[Fichier]){
        val rlist = new DefaultListModel()
        for(fichier <- fichiers){
            rlist addElement FichierDisplayItem(fichier)
        }
        getFileList.setModel(rlist)
    }

    def loadSupportTree(supports:List[Support]) = {
        val rootNode = new DefaultMutableTreeNode("supports");
        for(support <- supports){
            val node = new DefaultMutableTreeNode(SupportDisplayItem(support))
            rootNode add node
        }
        getSupportTree.setModel(new DefaultTreeModel(rootNode))
    }

    def addDossierToNode(dossiers:List[Dossier], supportNode:DefaultMutableTreeNode){
        for(dossier <- dossiers){
            val node = new DefaultMutableTreeNode(DossierDisplayItem(dossier))
            supportNode add node
        }
    }
    
    class InputKeyListener extends KeyAdapter{
        override
        def keyPressed(e:KeyEvent) = {
            if (e.getKeyCode == 10){
                searchCoordinator ! FichierSearchCriteria(getInputTextField getText)
            }
        }
    }

    class SupportTreeListener  extends TreeWillExpandListener {

        def treeWillExpand(event:TreeExpansionEvent)= {
            val node = event.getPath.getLastPathComponent.asInstanceOf[DefaultMutableTreeNode]
            node.getUserObject match {
                case SupportDisplayItem(support) => searchCoordinator ! LoadSupport(support.idSupport, node)
                case DossierDisplayItem(dossier) => searchCoordinator ! FichierParDossierCriteria(dossier.idDossier)
                case _ => 
            }
        }

        def treeWillCollapse(event:TreeExpansionEvent)={

        }
    }
}

case class DisplayItem
case class FichierDisplayItem(fichier:Fichier) extends DisplayItem { override def toString = fichier.nom}
case class SupportDisplayItem(support:Support) extends DisplayItem { override def toString = support.nom}
case class DossierDisplayItem(dossier:Dossier) extends DisplayItem { override def toString = dossier.nom}

case class LoadSupport(idSupport:Long, node:DefaultMutableTreeNode)
