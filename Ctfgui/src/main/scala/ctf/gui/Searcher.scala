package ctf.gui

import scala.actors._
import scala.actors.Actor._
import domain._
import CtrlMessages._

class Searcher {
    import CustomConversions.convertList

    def fileSearcher: Actor = actor {
        loop{
            react{
                case source:Any =>
                    source match {
                        case FichierSearchCriteria(name) =>
                            println("criteria=" + name)
                            sender ! FichierResult(findFichier(name),source)
                    }
            }
        }
    }

    def basicSearcher: Actor = actor {
        loop{
            react{
                case source:Any =>
                    source match {
                        case SearchAllSupport =>
                            println("recherche support...")
                            sender ! SupportResult(findAllSupport,source)
                        case DossierParSupportSearchCriteria(idSupport,node) =>
                            println("recherche dossier pour support "+idSupport+"...")
                            sender ! DossierResult(findDossierParSupport(idSupport),source)
                        case FichierParDossierCriteria(idDossier) =>
                            println("recherche fichier pour dossier "+idDossier+"...")
                            sender ! FichierResult(findFichierParDossier(idDossier),source)
                    }
            }
        }
    }

    private def findFichier(name:String):List[Fichier]=
    SqlMapConfig.sqlMapper.queryForList("rechercheFichier", "%"+name+"%")

    private def findFichierParDossier(idDossier:Long):List[Fichier]=
    SqlMapConfig.sqlMapper.queryForList("rechercheFichierParDossier", idDossier)

    private def findAllSupport:List[Support]=
    SqlMapConfig.sqlMapper.queryForList("rechercheToutSupport", None)

    private def findDossierParSupport(idSupport:Long):List[Dossier]=
    SqlMapConfig.sqlMapper.queryForList("rechercheDossierParSupport", idSupport)
}


