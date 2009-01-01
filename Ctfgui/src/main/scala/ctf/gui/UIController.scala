
package ctf.gui

import actors._
import actors.Actor._
import ui._

class UIController(env: {val searcher:Searcher
                         val finderView:FinderView
                         val browserView:BrowserView}) {

    def searchCoordinator:Actor = actor{
        loop{
            react{
                case criteria:CriteriaMessage =>
                    println("searchCoordinator ("+self+") -> "+criteria)
                    env.searcher.searcher ! criteria
                case result:ResultListMessage =>
                    println("searchCoordinator ("+self+") <- "+result.results.size + " results for "+result.source+" found")
                    result match {
                        case fichierResult:FichierResult if fichierResult.source.isInstanceOf[FinderCriteria] =>
                            env.finderView.loadResultFileList(fichierResult.fichiers)
                        case fichierResult:FichierResult if fichierResult.source.isInstanceOf[FichierParDossierCriteria] =>
                            env.browserView.loadFileList(fichierResult.fichiers)
                        case supportResult:SupportResult =>
                            env.browserView.loadSupportTree(supportResult.supports)
                        case dossierResult:DossierResult =>
                            val idDossierRacine = env.browserView.addDossierToNode(dossierResult.dossiers,dossierResult.source.asInstanceOf[DossierParSupportSearchCriteria].node)
                            env.searcher.searcher ! FichierParDossierCriteria(idDossierRacine)
                        case _ => throw new Exception("unknown result list "+ result)
                    }
                case message:Any => throw new Exception("unknown message "+message)
            }
        }
    }
}
