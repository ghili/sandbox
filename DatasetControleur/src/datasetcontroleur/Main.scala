package datasetcontroleur

import datasetcontroleur.TableIntrospection._
import datasetcontroleur.QueryBuilder._
import xml._
import collection.mutable._
import org.springframework.context.support._
import org.springframework.jdbc.core._
import org.springframework.transaction.support._
import org.springframework.transaction._
import org.springframework.jdbc.datasource.DataSourceTransactionManager

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) = {
    val context = new ClassPathXmlApplicationContext(Array[String]("config/config.xml"));
    val jdbcTemplate = context.getBean("DummyDao").asInstanceOf[DummyDao].getJdbcTemplate;
    val txManager = context.getBean("txManager").asInstanceOf[DataSourceTransactionManager]
    val txDef = new DefaultTransactionDefinition
    txDef.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW)

    val fileName = args(0)
    val dataset = XML.loadFile(fileName)
    val datas = dataset.child
    
    val txStatus = txManager.getTransaction(txDef)
    val dicoTable:HashMap[String,TypeParColonneType] = new HashMap()

    try{
      for(data <- datas if data.label !=  "#PCDATA"){
        val typesColonne:TypeParColonneType = dicoTable.get(data.label).getOrElse(memoColonnes(data.label, jdbcTemplate, dicoTable))
        val pairs = for (att:MetaData <-data.attributes if att.value.toString != "[NULL]") yield (att.key, att.value)
        val query = buildInsertQuery(data.label, pairs, typesColonne)
        println(query)
        jdbcTemplate.execute(query)
      }
    } finally{
      txManager.rollback(txStatus)
    }
    print("cool")
  }
}

