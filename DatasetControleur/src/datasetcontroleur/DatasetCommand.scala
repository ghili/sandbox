package datasetcontroleur

import xml._
import collection.mutable._
import org.springframework.jdbc.core.JdbcTemplate
import TableIntrospection._
import QueryBuilder._

class DatasetCommand(tableIntrospection:TableIntrospection) requires (DatasetCommand with DbTemplate){

  def insert(dataset:Elem)={
    val dicoTable = new HashMap[String,TypeParColonneType]()
    val datas = dataset child
    
    for(data <- datas if notPcData(data.label)){
      val typesColonne:TypeParColonneType = dicoTable get data.label getOrElse tableIntrospection.memoColonnes(data.label, extractSchemaTable, dicoTable)
      val pairs = for (att:MetaData <-data.attributes if notNullDbValue(att.value toString)) yield (att.key, att.value)
      val query = buildInsertQuery(data.label, pairs, typesColonne)
      println(query)
      execute(query)
    }
  }
  
  def delete(dataset:Elem)={
    val dicoTable = new HashMap[String,TypeParColonneType]()
    val dicoPk = new HashMap[String,List[String]]()
    val datas = dataset child
    val revdatas = datas reverse
    
    for(data <- revdatas if notPcData(data.label)){
      val typesColonne:TypeParColonneType = dicoTable get data.label getOrElse tableIntrospection.memoColonnes(data.label, extractSchemaTable, dicoTable)
      val colonnesPks = dicoPk.get(data.label).getOrElse(tableIntrospection.memoPK(data.label, extractSchemaTable, dicoPk))
      val pairs = for (att:MetaData <-data.attributes if notNullDbValue(att.value toString)) yield (att.key, att.value)
      val query = buildDeleteQuery(data.label, pairs, colonnesPks, typesColonne)
      println(query)
      execute(query)
    }
  }

  private def notPcData(value:String):Boolean = {
    value != "#PCDATA"
  }
  
  private def notNullDbValue(value:String):Boolean = {
    value != "[NULL]"
  }
  
  private def extractSchemaTable(label:String):Array[Object] = {
    val tableInfo:Array[String] = label split '.'
    Array[Object](tableInfo(0).toUpperCase, tableInfo(1).toUpperCase)    
  }
}