package datasetcontroleur

import collection.mutable._
import org.springframework.jdbc.core._
import java.util.{List=> JavaList}
import java.util.{Map=> JavaMap}

object TableIntrospection {
  
  type TypeParColonneType = HashMap[String,String]
  
  /**
   *
   */
  def memoColonnes(label:String, jdbcTemplate: JdbcTemplate, dicColonne:HashMap[String,TypeParColonneType]):TypeParColonneType = {
    val pos = label.indexOf(".")
    //val tableInfo:Array[String] = label.split(".")
    val schema = label.substring(0,pos)
    val tabName = label.substring(pos+1)
    val colonnes:JavaList[_] = jdbcTemplate.queryForList("SELECT colName, typeName FROM SYSCAT.COLUMNS WHERE tabSchema =? AND tabName = ?", Array[Object](schema, tabName))
    val typesColonne:TypeParColonneType = new TypeParColonneType

    val ite = colonnes.iterator
    while(ite.hasNext){
      val entree = ite.next.asInstanceOf[JavaMap[String, String]]
      typesColonne += entree.get("COLNAME") -> entree.get("TYPENAME")
    }
    dicColonne += label -> typesColonne
    return typesColonne
  }
}
