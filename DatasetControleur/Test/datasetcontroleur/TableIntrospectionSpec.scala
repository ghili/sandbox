package datasetcontroleur

import org.specs._
import org.specs.mock._
import java.util.{List => JavaList}

object Db2TableIntrospectionSpec extends Specification with JMocker {

  //@todo créer un mock avec jmock
  trait MockDbTemplate extends DbTemplate {
    def execute(query:String) = ()  
    def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T] = List()
    def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T] = List()
  }

  "le composant db2" should {
    "definir la requête pour récupérer les types de chaque colonne d'une table" in {
      val db2Component = new Db2TableIntrospectionComponent with MockDbTemplate
      db2Component.tableIntrospection.SELECT_TYPE_COLUMNS must notBeNull
    }
    
  "definir la requête pour récupérer les pks d'une table" in {
      val db2Component = new Db2TableIntrospectionComponent with MockDbTemplate
      db2Component.tableIntrospection.SELECT_COLUMNPK must notBeNull
    }
  }
}
