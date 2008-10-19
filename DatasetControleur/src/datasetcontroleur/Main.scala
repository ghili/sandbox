package datasetcontroleur

import xml._
import collection.mutable._
import org.springframework.context.support._
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.transaction.support._
import org.springframework.transaction._
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import io.Source
import java.util.{List=> JavaList}


trait DbTemplate {
  def execute(query:String)  
  def queryForList(query:String, params:Array[Object]):JavaList[_]
  def queryForList(query:String, params:Array[Object], returnType:Class[_]):JavaList[_]
}

trait SpringJdbcTemplate extends DbTemplate {
  var dbTemplate:JdbcTemplate = null

  def execute(query:String) = dbTemplate execute query
  def queryForList(query:String, params:Array[Object]):JavaList[_] =  dbTemplate.queryForList(query,params)
  def queryForList(query:String, params:Array[Object], returnType:Class[_]):JavaList[_] =  dbTemplate.queryForList(query,params,returnType)
}

object TableIntrospection {type TypeParColonneType = HashMap[String,String]}
abstract class TableIntrospection  requires (TableIntrospection with DbTemplate) {
  import TableIntrospection.TypeParColonneType
  def memoColonnes[A](label:A, extract:A => Array[Object],  dicColonne:HashMap[A,TypeParColonneType]):TypeParColonneType
  def memoPK[A](label:A, extract:A => Array[Object], dicColonnePk:HashMap[A,List[String]]):List[String]
}

/*
 abstract class AbstractDatasetControleur {
  type DbTemplate <: AbstractDbTemplate
  type QueryBuilder <: AbstractQueryBuilder
  type TableIntrospection <: AbstractTableIntrospection

  abstract class AbstractCommand {
    def insert(dataset:Elem)
    def delete(dataset:Elem)
  }
}

object DatasetControleur extends AbstractDatasetControleur{
  type DbTemplate = SpringJdbcTemplate
}
*/
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

    //lecture du fichier et récupération de la ligne définissant le dataset
    val lines = for {
      (line) <- Source fromFile(file) getLines
    } yield (line)
    val line = lines.find({x:String => x.indexOf("dataset") != -1})
    val fileName = line.getOrElse(throw new Exception("dataset file name not found")).split("=")(1)
    
    val dataset = XML loadFile(fileName)
    val tableIntrospection = new Db2TableIntrospection with SpringJdbcTemplate
    val command = new DatasetCommand(tableIntrospection) with SpringJdbcTemplate
    tableIntrospection.dbTemplate = jdbcTemplate
    command.dbTemplate = jdbcTemplate

    if ("testInsert" == action){
      val txStatus = txManager getTransaction(txDef)
      try{
        command insert dataset
      } finally{
        txManager rollback(txStatus)
      }
    }else if("delete" == action){
      command delete dataset
    }else if("insert" ==action){
        command insert dataset
    }
    print("cool ('"+action+"' lines from "+ fileName +")")
  }
}