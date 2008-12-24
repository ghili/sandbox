
package ctf.gui

import actors._
import actors.Actor._
import ctf.gui.ui._
import CtrlMessages._

class UIController {

    var browserAction:BrowserAction = _
    var finderAction:FinderAction = _

    val searcher = new Searcher
    var nActor = 0

    def searchCoordinator:Actor = actor{
        nActor += 1
        println("actor"+nActor)
        loop{
            react{
                case fichierCriteria:FichierSearchCriteria =>
                    searcher.fileSearcher ! fichierCriteria
                case SearchAllSupport =>
                    searcher.basicSearcher ! SearchAllSupport
                case LoadSupport(idSupport,node) =>
                    searcher.basicSearcher ! DossierParSupportSearchCriteria(idSupport)
                    println("waiting"+nActor)
                    receiveWithin(50000) {
                        case dossierResult:DossierResult =>
                            println(dossierResult.dossiers.size + " dossiers found")
                            browserAction.addDossierToNode(dossierResult.dossiers,node)
                    }
                    println("abandonned"+nActor)
                case fichierParDossierCriteria:FichierParDossierCriteria =>
                    searcher.basicSearcher ! fichierParDossierCriteria
                    println("waiting"+nActor)
                    receiveWithin(50000) {
                        case fichierResult:FichierResult =>
                            println(fichierResult.fichiers.size + " fichiers found")
                            browserAction.loadFileList(fichierResult.fichiers)
                    }
                    println("abandonned"+nActor)
                case fichierResult:FichierResult =>
                    println(fichierResult.fichiers.size + " files found")
                    finderAction.loadResultFileList(fichierResult.fichiers)
                    exit('stop)
                case supportResult:SupportResult =>
                    println(supportResult.supports.size + " supports found")
                    browserAction.loadSupportTree(supportResult.supports)

            }
        }
    }
}
