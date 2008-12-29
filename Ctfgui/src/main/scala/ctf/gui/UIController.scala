
package ctf.gui

import actors._
import actors.Actor._
import ui._

class UIController {
    import Searcher._

    var browserAction:BrowserAction = _
    var finderView:FinderView = _

    def searchCoordinator:Actor = actor{
        loop{
            react{
                case criteria:CriteriaMessage =>
                    println("searchCoordinator ("+self+") -> "+criteria)
                    searcher ! criteria
                case result:ResultListMessage =>
                    println("searchCoordinator ("+self+") <- "+result.results.size + " results for "+result.source+" found")
                    result match {
                        case fichierResult:FichierResult if fichierResult.source.isInstanceOf[FinderCriteria] =>
                            finderView.loadResultFileList(fichierResult.fichiers)
                        case fichierResult:FichierResult if fichierResult.source.isInstanceOf[FichierParDossierCriteria] =>
                            browserAction.loadFileList(fichierResult.fichiers)
                        case supportResult:SupportResult =>
                            browserAction.loadSupportTree(supportResult.supports)
                        case dossierResult:DossierResult =>
                            val idDossierRacine = browserAction.addDossierToNode(dossierResult.dossiers,dossierResult.source.asInstanceOf[DossierParSupportSearchCriteria].node)
                            searcher ! FichierParDossierCriteria(idDossierRacine)
                        case _ => throw new Exception("unknown result list "+ result)
                    }
                 case message:Any => throw new Exception("unknown message "+message)
            }
        }
    }
}
