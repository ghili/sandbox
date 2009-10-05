
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
                        case fichierResult @ FichierResult(_,_:FinderCriteria) =>
                            env.finderView.loadResultFileList(fichierResult.results)
                        case fichierResult @ FichierResult(_,_:FichierParDossierCriteria) =>
                            env.browserView.loadFileList(fichierResult.results)
                        case supportResult:SupportResult =>
                            env.browserView.loadSupportTree(supportResult.results)
                        case dossierResult:DossierResult =>
                            val idDossierRacine = env.browserView.addDossierToNode(dossierResult.results,dossierResult.source.asInstanceOf[DossierParSupportSearchCriteria].node)
                            env.searcher.searcher ! FichierParDossierCriteria(idDossierRacine)
                        case _ => throw new Exception("unknown result list "+ result)
                    }
                case message:Any => throw new Exception("unknown message "+message)
            }
        }
    }
}
