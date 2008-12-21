package ctf.gui

import scala.actors._
import scala.actors.Actor._
import domain._

class Searcher {
    import CustomConversions.convertList

    def fileSearcher: Actor = actor {
        loop{
            react{
                case FichierSearchCriteria(name) =>
                    println("criteria=" + name)
                    sender ! FichierResult(findFichier(name))
            }
        }
    }

    def basicSearcher: Actor = actor {
        loop{
            react{
                case SearchAllSupport =>
                    println("recherche support...")
                    sender ! SupportResult(findAllSupport)
                case DossierParSupportSearchCriteria(idSupport) =>
                    println("recherche dossier pour support "+idSupport+"...")
                    sender ! DossierResult(findDossierParSupport(idSupport))
                case FichierParDossierCriteria(idDossier) =>
                    println("recherche fichier pour dossier "+idDossier+"...")
                    sender ! FichierResult(findFichierParDossier(idDossier))
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

case class FichierSearchCriteria(name:String)

case class FichierParDossierCriteria(idDossier:Long)

case class DossierParSupportSearchCriteria(idSupport:Long)

case class SearchAllSupport

case class SupportResult(supports:List[Support])

case class FichierResult(fichiers:List[Fichier])

case class DossierResult(dossiers:List[Dossier])
