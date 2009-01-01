package ctf.gui

import scala.actors._
import scala.actors.Actor._
import com.ibatis.sqlmap.client.SqlMapClient
import domain._

class Searcher(env:{val sqlMapper:SqlMapClient}) {
    import CustomConversions.convertList

    def searcher: Actor = actor {
        loop{
            react{
                case source:Any =>
                    println("searcher (" + self + ") <- "+source)
                    source match {
                        case criteria:FinderCriteria =>
                            sender ! FichierResult(findFichier(FinderCriteriaAdapter(criteria)),source)
                        case SearchAllSupport =>
                            sender ! SupportResult(findAllSupport,source)
                        case DossierParSupportSearchCriteria(idSupport,node) =>
                            sender ! DossierResult(findDossierParSupport(idSupport),source)
                        case FichierParDossierCriteria(idDossier) =>
                            sender ! FichierResult(findFichierParDossier(idDossier),source)
                    }
            }
        }
    }

    private def findFichier(criteria:FinderCriteriaAdapter):List[Fichier]=
    env.sqlMapper.queryForList("rechercheFichier",criteria)

    private def findFichierParDossier(idDossier:Long):List[Fichier]=
    env.sqlMapper.queryForList("rechercheFichierParDossier", idDossier)

    private def findAllSupport:List[Support]=
    env.sqlMapper.queryForList("rechercheToutSupport", None)

    private def findDossierParSupport(idSupport:Long):List[Dossier]=
    env.sqlMapper.queryForList("rechercheDossierParSupport", idSupport)
}

import scala.reflect.BeanProperty

case class FinderCriteriaAdapter(private val criteria:FinderCriteria){

    @BeanProperty
    val name = "%"+criteria.value+"%"

    @BeanProperty
    val minSize = getSize(SizeCriteriaEnumeration.mini)

    @BeanProperty
    val maxSize = getSize(SizeCriteriaEnumeration.maxi)

    private def getSize(typeCriteria:SizeCriteriaEnumeration.Value):Double={
        val searchOption = criteria.options.find{(_:SearchOption).sizeCriteria==typeCriteria}
        searchOption match {
            case Some(o) => (o.sizeUnit  match {
                        case SizeUnitEnumeration.ko => o.number * 1000
                        case SizeUnitEnumeration.mo => o.number * 1000000
                        case SizeUnitEnumeration.go => o.number * 1000000000
                    })
            case _ => 0
        }
    }

}
