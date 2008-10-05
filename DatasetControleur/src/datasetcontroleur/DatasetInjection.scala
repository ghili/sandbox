package datasetcontroleur

import xml._
import collection.mutable._
import org.springframework.jdbc.core.JdbcTemplate
import TableIntrospection._
import QueryBuilder._

class DatasetInjection  {

  def inject(dataset:Elem, jdbcTemplate:JdbcTemplate)={
    val dicoTable:HashMap[String,TypeParColonneType] = new HashMap()
    val datas = dataset.child
    
    for(data <- datas if data.label !=  "#PCDATA"){
      val typesColonne:TypeParColonneType = dicoTable.get(data.label).getOrElse(memoColonnes(data.label, jdbcTemplate, dicoTable))
      val pairs = for (att:MetaData <-data.attributes if att.value.toString != "[NULL]") yield (att.key, att.value)
      val query = buildInsertQuery(data.label, pairs, typesColonne)
      println(query)
      jdbcTemplate.execute(query)
    }
  }
}
