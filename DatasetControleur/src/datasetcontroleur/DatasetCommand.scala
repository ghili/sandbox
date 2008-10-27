package datasetcontroleur

import xml._
import collection.mutable._
import org.springframework.jdbc.core.JdbcTemplate
  
trait DatasetCommandComponent { this:TableIntrospectionComponent with DbTemplate =>
  import TableIntrospection.TypeParColonneType
  val queryBuilder:QueryBuilder

  class DatasetCommand {

    def insert(dataset:Elem)={
      val dicoTable = new HashMap[String,TypeParColonneType]
      val datas = dataset child

      for(data <- datas if notPcData(data.label)){
        val typesColonne:TypeParColonneType = dicoTable get data.label getOrElse tableIntrospection.memoColonnes(data.label, dicoTable)
        val pairs = for (att:MetaData <-data.attributes if notNullDbValue(att.value toString)) yield (att.key, att.value)
        val query = queryBuilder.buildInsertQuery(data.label, pairs, typesColonne)
        execute(query)
      }
    }

    def delete(dataset:Elem)={
      val dicoTable = new HashMap[String,TypeParColonneType]
      val dicoPk = new HashMap[String,List[String]]
      val revdatas = (dataset child) reverse

      for(data <- revdatas if notPcData(data.label)){
        val typesColonne:TypeParColonneType = dicoTable get data.label getOrElse tableIntrospection.memoColonnes(data.label, dicoTable)
        val colonnesPks = dicoPk.get(data.label).getOrElse(tableIntrospection.memoPK(data.label, dicoPk))
        val pairs = for (att:MetaData <-data.attributes if notNullDbValue(att.value toString)) yield (att.key, att.value)
        val query = queryBuilder.buildDeleteQuery(data.label, pairs, colonnesPks, typesColonne)
        execute(query)
      }
    }

    private def notPcData = (_:String) != "#PCDATA"

    private def notNullDbValue = (_:String) != "[NULL]"
  }
}