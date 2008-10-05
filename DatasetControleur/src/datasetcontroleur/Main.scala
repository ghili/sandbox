package datasetcontroleur

import xml._
import collection.mutable._
import org.springframework.context.support._
import org.springframework.jdbc.core.JdbcTemplate
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
    val datasetInjection = new DatasetInjection

    val txStatus = txManager.getTransaction(txDef)
    try{
      datasetInjection.inject(dataset, jdbcTemplate)
    } finally{
      txManager.rollback(txStatus)
    }
    print("cool")
  }
}

