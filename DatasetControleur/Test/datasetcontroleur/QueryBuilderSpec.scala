
package datasetcontroleur

import org.specs._
import org.specs.mock._
import TableIntrospection.TypeParColonneType

object QueryBuilderSpec extends Specification {

  "the query builder" should {
    "be able to create an insert query" in {
      new QueryBuilder().buildInsertQuery("nomTable", List(), new TypeParColonneType) must equalIgnoreCase("INSERT INTO nomTable () VALUES ()")
    }
    
    "be able to create a delete query" in {
      new QueryBuilder().buildDeleteQuery("nomTable", List(), List(), new TypeParColonneType) must equalIgnoreCase("DELETE FROM nomTable WHERE ")
    }
  }
}
