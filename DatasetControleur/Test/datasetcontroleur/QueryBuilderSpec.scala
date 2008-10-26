
package datasetcontroleur

import org.specs._
import org.specs.mock._
import org.specs.runner.JUnit4
import TableIntrospection.TypeParColonneType

object QueryBuilderSpec extends Specification {

  "the query builder" should {
    "be able to create an insert query" in {
      val typeParColonne = new TypeParColonneType
      new QueryBuilder().buildInsertQuery("nomTable", List(), typeParColonne) must equalIgnoreCase("INSERT INTO nomTable () VALUES ()")
    }
  }
}
