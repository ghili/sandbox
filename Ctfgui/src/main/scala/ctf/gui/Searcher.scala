package ctf.gui

import scala.actors._
import scala.actors.Actor._
import domain._

object Searcher {
    import CustomConversions.convertList

    def searcher: Actor = actor {
        loop{
            react{
                case source:Any =>
                    println("searcher (" + self + ") <- "+source)
                    source match {
                        case FinderCriteria(name,options) =>
                            sender ! FichierResult(findFichier(name),source)
                        case SearchAllSupport() =>
                            sender ! SupportResult(findAllSupport,source)
                        case DossierParSupportSearchCriteria(idSupport,node) =>
                            sender ! DossierResult(findDossierParSupport(idSupport),source)
                        case FichierParDossierCriteria(idDossier) =>
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


