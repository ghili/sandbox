package datasetcontroleur

import collection.mutable._
import TableIntrospection.TypeParColonneType
import org.specs._
import org.specs.mock._
import java.util.{List => JavaList}

/*trait MockDbTemplate extends DbTemplate with Mocker{
 override def execute(query:String) = record
 override def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T] = recordAndReturn(List())
 override def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T] = recordAndReturn(List())
 }*/

trait MockDbTemplate extends DbTemplate with JMocker{
  var dbTemplate= mock(classOf[DbTemplate])
  override def execute(query:String) = dbTemplate.execute(query)
  override def queryForList[T <: AnyRef](query:String, params:Array[Object]):List[T] = dbTemplate.queryForList(query, params)
  override def queryForList[T <: AnyRef](query:String, params:Array[Object], returnType:Class[_]):List[T] = dbTemplate.queryForList(query, params, returnType)
}

object Db2TableIntrospectionSpec extends Specification with Db2TableIntrospectionComponent with MockDbTemplate {

  "the component for db2" should {
    "define the query to collect columns' type of a table" in {
      tableIntrospection.SELECT_TYPE_COLUMNS must notBeNull
    }
    
    "define que query to collect the primary key of a table" in {
      tableIntrospection.SELECT_COLUMNPK must notBeNull
    }
  }

  "calling the method to find column's type component" should {
    doBefore { dbTemplate = mock(classOf[DbTemplate]) }
    "call the right query" in {
      expect {
        one(dbTemplate).queryForList(tableIntrospection.SELECT_TYPE_COLUMNS, Array("SCHEMA", "TABLE")) willReturn List()
      }
      tableIntrospection.memoColonnes("SCHEMA.TABLE", new HashMap[String,TypeParColonneType])
    }
  }
}
