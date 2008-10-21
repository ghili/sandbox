package datasetcontroleur

import xml._
import collection.mutable._
import org.springframework.context.support._
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.transaction.support._
import org.springframework.transaction._
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import io.Source
import TableIntrospection.TypeParColonneType
import java.util.{List=> JavaList}


//Composants abstraits
trait DbTemplate {
  def execute(query:String)  
  def queryForList(query:String, params:Array[Object]):JavaList[_]
  def queryForList(query:String, params:Array[Object], returnType:Class[_]):JavaList[_]
}

//implémentations
trait SpringJdbcTemplate extends DbTemplate {
  var dbTemplate:JdbcTemplate = null
  def execute(query:String) = {println(query)
                               dbTemplate execute query}
  def queryForList(query:String, params:Array[Object]):JavaList[_] =  dbTemplate.queryForList(query,params)
  def queryForList(query:String, params:Array[Object], returnType:Class[_]):JavaList[_] =  dbTemplate.queryForList(query,params,returnType)
}

trait Db2TableIntrospectionComponent extends TableIntrospectionComponent{ this: DbTemplate=>
  val tableIntrospection = new Db2TableIntrospection

  class Db2TableIntrospection extends TableIntrospection {
    val SELECT_TYPE_COLUMNS = "SELECT colName, typeName FROM SYSCAT.COLUMNS WHERE tabSchema = ? AND tabName = ?"
    val SELECT_COLUMNPK = """ SELECT kc.colName FROM SYSCAT.KEYCOLUSE kc
                              JOIN SYSCAT.TABCONST cst ON cst.constName = kc.constName AND cst.tabSchema = kc.tabSchema AND cst.tabName = kc.tabName
                              WHERE kc.tabSchema = ? AND kc.tabName = ? AND cst.type = 'P'"""
  }
}

class DatasetControleur requires (DatasetControleur with DbTemplate with TableIntrospectionComponent) extends DatasetCommandComponent {
  val datasetCommand = new DatasetCommand
  val queryBuilder = new QueryBuilder
}

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) = {
    val context = new ClassPathXmlApplicationContext(Array[String]("config/config.xml"))
    val jdbcTemplate = (context getBean "DummyDao").asInstanceOf[DummyDao].getJdbcTemplate
    val txManager = (context getBean "txManager").asInstanceOf[DataSourceTransactionManager]
    val txDef = new DefaultTransactionDefinition
    txDef.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW)

    val action = args(0)
    
    val file = this.getClass().getClassLoader getResource "config/proprietes" getFile

    //lecture du fichier de configuration et récupération de la ligne définissant le dataset
    val lines = for {
      (line) <- Source fromFile(file) getLines
    } yield (line)
    val line = lines.find({x:String => x.indexOf("dataset") != -1})
    val fileName = line.getOrElse(throw new Exception("dataset file name not found")).split("=")(1)
    
    val dataset = XML loadFile(fileName)
    val controleur = new DatasetControleur with SpringJdbcTemplate with Db2TableIntrospectionComponent
    controleur.dbTemplate = jdbcTemplate

    if ("testInsert" == action){
      val txStatus = txManager getTransaction(txDef)
      try{
        controleur.datasetCommand insert dataset
      } finally{
        txManager rollback(txStatus)
      }
    }else if("delete" == action){
      controleur.datasetCommand delete dataset
    }else if("insert" ==action){
        controleur.datasetCommand insert dataset
    }
    print("cool ('"+action+"' lines from "+ fileName +")")
  }
}