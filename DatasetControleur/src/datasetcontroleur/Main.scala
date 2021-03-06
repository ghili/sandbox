package datasetcontroleur

import xml._
import io.Source
import collection.mutable._
import collection.jcl.BufferWrapper
import org.springframework.context.support._
import org.springframework.context.ApplicationContext
import org.springframework.transaction.support._
import org.springframework.transaction._
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import java.util.{List=> JavaList}

object CustomConversions {
    implicit def convertList[T](javaList:JavaList[_]):List[T] = new BufferWrapper[T]{def underlying = javaList.asInstanceOf[JavaList[T]]}.toList
}

trait AbstractSpringJdbcTemplateComponent extends DbTemplateComponent{
    class SpringJdbcTemplate(jdbcTemplate:JdbcTemplate) extends DbTemplate {
        import CustomConversions.convertList

        override
        def execute(query:String) = {
            println(query)
            jdbcTemplate execute query
        }

        override
        def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T] =
        jdbcTemplate.queryForList(query,params)

        override
        def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T] =
        jdbcTemplate.queryForList(query,params,returnType)
    }
}
  

trait Db2TableIntrospectionComponent extends TableIntrospectionComponent{ this: DbTemplateComponent =>
    lazy val tableIntrospection = new Db2TableIntrospection

    class Db2TableIntrospection extends TableIntrospection {
        val SELECT_TYPE_COLUMNS = "SELECT colName, typeName FROM SYSCAT.COLUMNS WHERE tabSchema = ? AND tabName = ?"
        val SELECT_COLUMNPK = """ SELECT kc.colName FROM SYSCAT.KEYCOLUSE kc
                              JOIN SYSCAT.TABCONST cst ON cst.constName = kc.constName AND cst.tabSchema = kc.tabSchema AND cst.tabName = kc.tabName
                              WHERE kc.tabSchema = ? AND kc.tabName = ? AND cst.type = 'P'"""
    }
}

object Main {

    // Classe agrégant les composants
    class DatasetControleur extends DatasetCommandComponent { this: DbTemplateComponent with TableIntrospectionComponent  =>
        lazy val datasetCommand = new DatasetCommand
        lazy val queryBuilder = new QueryBuilder
    }

    /**
     * @param args the command line arguments
     */
    def main(args: Array[String]) = {

        //lecture du fichier de configuration et récupération de la ligne définissant le dataset si le fichier n'est pas donné en argument
        val datasetFileName = if (args.length < 2){
            val file = this.getClass().getClassLoader getResource "config/proprietes" getFile
      
            (Source fromFile(file) getLines)
            .find ((_:String) startsWith "dataset=")
            .getOrElse(throw new Exception("dataset file name not found")).split("=")(1)
            .stripLineEnd
        } else args(1)
        lazy val dataset = XML loadFile datasetFileName

        val context = new ClassPathXmlApplicationContext(Array[String]("config/config.xml"))
        trait SpringJdbcTemplateComponent extends AbstractSpringJdbcTemplateComponent{
            lazy val dbTemplate = new SpringJdbcTemplate((context getBean "DummyDao").asInstanceOf[DummyDao].getJdbcTemplate)
        }
        val controleur = new DatasetControleur with SpringJdbcTemplateComponent with Db2TableIntrospectionComponent

        (args(0) match{
                case "testInsert" => noCommit(context, controleur.datasetCommand insert _ ) _
                case "delete" => controleur.datasetCommand delete _
                case "insert" => controleur.datasetCommand insert _
                case _ => throw new RuntimeException("Je n'exécute aucune action par défaut")
            })(dataset)
    
        print("cool ('"+args(0)+"' lines from "+ datasetFileName +")")
    }

    /**
     * Effectue une action en transaction en rollbackant à la fin
     */
    private def noCommit (context:ApplicationContext, action: Elem => Unit) (element:Elem) = {
        val txManager = (context getBean "txManager").asInstanceOf[DataSourceTransactionManager]
        val txDef = new DefaultTransactionDefinition
        txDef.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW)
        val txStatus = txManager getTransaction txDef
        try{
            action(element)
        } finally {
            println("rollback")
            txManager rollback txStatus
        }
    }
}