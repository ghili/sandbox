package ctf.gui.ui

import org.specs._
import org.specs.runner._

class FinderCriteriaBuilderTest extends JUnit4(FinderCriteriaBuilderSpec)

object FinderCriteriaBuilderSpec extends Specification {

    "the criteria builder" should {
        "be able to parse a size unit" in {
            FinderCriteriaBuilder.parse("ko", FinderCriteriaBuilder.sizeUnit) mustEqual SizeUnitEnumeration.ko
        }

        "be able to parse a size criteria" in {
            FinderCriteriaBuilder.parse("mini", FinderCriteriaBuilder.sizeCriteria) mustEqual SizeCriteriaEnumeration.mini
        }

        "be able to parse an option" in {
            FinderCriteriaBuilder.parse("de 5 ko mini", FinderCriteriaBuilder.searchOption) mustEqual SearchOption(5, SizeUnitEnumeration.ko, SizeCriteriaEnumeration.mini)
        }

        "be able to parse a simple name" in {
            FinderCriteriaBuilder.parse("machin", FinderCriteriaBuilder.searchCriteria) mustEqual FinderCriteria("machin",Nil)
        }

        "be able to parse a simple criteria" in {
            FinderCriteriaBuilder.parse("trouver \"machin\"", FinderCriteriaBuilder.searchCriteria) mustEqual FinderCriteria("machin",Nil)
        }

        "be able to parse a criteria with one option" in {
            FinderCriteriaBuilder.parse("trouver \"machin\" de 5 ko mini", FinderCriteriaBuilder.searchCriteria) mustEqual FinderCriteria("machin",List(SearchOption(5, SizeUnitEnumeration.ko, SizeCriteriaEnumeration.mini)))
        }
    }
    
}
