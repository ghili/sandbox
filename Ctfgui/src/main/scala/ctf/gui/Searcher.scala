package ctf.gui

import scala.actors._
import scala.actors.Actor._
import domain._

class Searcher {
    import CustomConversions.convertList

    def fileSearcher: Actor = actor {
        loop{
            react{
                case FileSearchCriteria(name) =>
                    sender ! findFiles(name)
            }
        }
    }

    def findFiles(name:String):List[Fichier]= SqlMapConfig.sqlMapper.queryForList("rechercheFichier", "%"+name+"%")
}

case class FileSearchCriteria(name:String)
