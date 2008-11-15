package datasetcontroleur

import xml._
import collection.mutable._
import scalaz.memo.Memo
import scalaz.memo.Memo._
import org.springframework.jdbc.core.JdbcTemplate
  
trait DatasetCommandComponent { this:TableIntrospectionComponent with DbTemplateComponent =>
    import TableIntrospection.TypeParColonneType
    val queryBuilder:QueryBuilder

    class DatasetCommand {

        def insert(dataset:Elem)={
            val memType = immutableHashMapMemo[String,TypeParColonneType]
            val datas = dataset child

            for(data <- datas if notPcData(data.label)){
                val pairs = for (att:MetaData <-data.attributes if notNullDbValue(att.value toString)) yield (att.key, att.value)
                dbTemplate.execute(queryBuilder.buildInsertQuery(data.label, pairs,
                                                                 memType(tableIntrospection.rechercherTypeColonne _)(data.label)))
            }
        }

        def delete(dataset:Elem)={
            val memType = immutableHashMapMemo[String,TypeParColonneType]
            val memPk = immutableHashMapMemo[String,List[String]]
            val reversedDatas = (dataset child) reverse

            for(data <- reversedDatas if notPcData(data.label)){
                val pairs = for (att:MetaData <-data.attributes if notNullDbValue(att.value toString)) yield (att.key, att.value)
                dbTemplate.execute(queryBuilder.buildDeleteQuery(data.label, pairs, 
                                                                 memPk(tableIntrospection.rechercherPk _)(data.label),
                                                                 memType(tableIntrospection.rechercherTypeColonne _)(data.label)))
            }
        }

        private def notPcData = (_:String) != "#PCDATA"

        private def notNullDbValue = (_:String) != "[NULL]"
    }
}