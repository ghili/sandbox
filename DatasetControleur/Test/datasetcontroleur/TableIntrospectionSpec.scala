package datasetcontroleur

import collection.mutable._
import TableIntrospection.TypeParColonneType
import org.specs._
import org.specs.mock._

/*trait MockDbTemplate extends DbTemplate with Mocker{
 override def execute(query:String) = record
 override def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T] = recordAndReturn(List())
 override def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T] = recordAndReturn(List())
 }*/

trait MockDb2TableIntrospectionComponent extends JMocker {
  var component:Db2TableIntrospectionComponent with MockDbTemplateComponent = _
  def init = {
    component = new Db2TableIntrospectionComponent with MockDbTemplateComponent
  }
  trait MockDbTemplateComponent extends DbTemplateComponent{
    val dbTemplate = mock(classOf[DbTemplate])  
  }
}

object Db2TableIntrospectionSpec extends Specification with MockDb2TableIntrospectionComponent {
  
  "the component for db2" should {
    doBefore{init}
    "define the query to collect columns' type of a table" in {
      component.tableIntrospection.SELECT_TYPE_COLUMNS must notBeNull
    }
    
    "define que query to collect the primary key of a table" in {
      component.tableIntrospection.SELECT_COLUMNPK must notBeNull
    }
  }

  "calling the method to find column's type component" should {
    doBefore { init}
    "call the right query" in {
      expect {
        one( component.dbTemplate).queryForList( component.tableIntrospection.SELECT_TYPE_COLUMNS, Array("SCHEMA", "TABLE")) willReturn List()
      }
      component.tableIntrospection.memoColonnes("SCHEMA.TABLE", new HashMap[String,TypeParColonneType])
    }
  }
}
