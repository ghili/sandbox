
package ctf.gui

import actors._
import actors.Actor._
import ctf.gui.ui._
import CtrlMessages._

class UIController {

    var browserAction:BrowserAction = _
    var finderAction:FinderAction = _

    val searcher = new Searcher

    def searchCoordinator:Actor = actor{
        loop{
            react{
                case criteria:CriteriaMessage =>
                    println("searchCoordinator ("+self+") -> "+criteria)
                    searcher.searcher ! criteria
                case result:ResultListMessage =>
                    println("searchCoordinator ("+self+") <- "+result.results.size + " results for "+result.source+" found")
                    result match {
                        case fichierResult:FichierResult if fichierResult.source.isInstanceOf[FichierSearchCriteria] =>
                            finderAction.loadResultFileList(fichierResult.fichiers)
                        case fichierResult:FichierResult if fichierResult.source.isInstanceOf[FichierParDossierCriteria] =>
                            browserAction.loadFileList(fichierResult.fichiers)
                        case supportResult:SupportResult =>
                            browserAction.loadSupportTree(supportResult.supports)
                        case dossierResult:DossierResult =>
                            browserAction.addDossierToNode(dossierResult.dossiers,dossierResult.source.asInstanceOf[DossierParSupportSearchCriteria].node)
                        case _ => throw new Exception("unknown result list "+ result)
                    }
                 case message:Any => throw new Exception("unknown message "+message)
            }
        }
    }
}
