package datasetcontroleur

import xml._
import collection.mutable._
import collection.jcl.BufferWrapper
import org.springframework.context.support._
import org.springframework.context.ApplicationContext
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
  def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T]
  def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T]
}

//Implémentations
trait SpringJdbcTemplate extends DbTemplate {
  var dbTemplate:JdbcTemplate = null
  def execute(query:String) = {
    println(query)
    dbTemplate execute query
  }
  def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T] =
  new BufferWrapper[T]{def underlying = dbTemplate.queryForList(query,params).asInstanceOf[JavaList[T]]}.toList

  def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T] = 
  new BufferWrapper[T]{def underlying = dbTemplate.queryForList(query,params,returnType).asInstanceOf[JavaList[T]]}.toList
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

object Main {

  // Classe agrégant les composants
  class DatasetControleur { this: DbTemplate with TableIntrospectionComponent with DatasetCommandComponent =>
    val datasetCommand = new DatasetCommand
    val queryBuilder = new QueryBuilder
  }

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) = {
    val context = new ClassPathXmlApplicationContext(Array[String]("config/config.xml"))

    //lecture du fichier de configuration et récupération de la ligne définissant le dataset si le fichier n'est pas donné en argument
    val datasetFileName = if (args.length < 2){
      val file = this.getClass().getClassLoader getResource "config/proprietes" getFile
      
      //@todo corriger bug quand le nom du fichier est suivi d'un saut de ligne
      (Source fromFile(file) getLines)
      .find {(_:String) startsWith "dataset="}
      .getOrElse(throw new Exception("dataset file name not found")).split("=")(1)
    } else args(1)
    
    
    val dataset = XML loadFile datasetFileName
    val controleur = new DatasetControleur with SpringJdbcTemplate with Db2TableIntrospectionComponent with DatasetCommandComponent
    controleur.dbTemplate = (context getBean "DummyDao").asInstanceOf[DummyDao] getJdbcTemplate

    (args(0) match{
        case "testInsert" => noCommit(context, controleur.datasetCommand insert _)
        case "delete" => controleur.datasetCommand delete _
        case "insert" => controleur.datasetCommand insert _
        case _ => throw new RuntimeException("Je n'exécute aucune action par défaut")
      })(dataset)
    
    print("cool ('"+args(0)+"' lines from "+ datasetFileName +")")
  }

  /**
   * Effectue une action en transaction en rollbackant à la fin
   */
  private def noCommit (context:ApplicationContext, action: Elem => Unit) = {
    val txManager = (context getBean "txManager").asInstanceOf[DataSourceTransactionManager]
    val txDef = new DefaultTransactionDefinition
    txDef.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW)
    val txStatus = txManager getTransaction txDef
    try{
      action
    } finally {
      txManager rollback txStatus
    }
  }
}