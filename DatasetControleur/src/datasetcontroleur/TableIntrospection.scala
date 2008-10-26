package datasetcontroleur

import collection.mutable._
import org.springframework.jdbc.core._
import java.util.{List=> JavaList}
import java.util.{Map=> JavaMap}

object TableIntrospection {type TypeParColonneType = HashMap[String,String]}

trait TableIntrospectionComponent { this: DbTemplate=>
  val tableIntrospection:TableIntrospection
  
  abstract class TableIntrospection {
    import TableIntrospection.TypeParColonneType
    val SELECT_TYPE_COLUMNS:String
    val SELECT_COLUMNPK:String

    /**
     *
     */
    def memoColonnes[A](label:A, extract:A => Array[Object],  dicColonne:HashMap[A,TypeParColonneType]):TypeParColonneType = {
      val typesColonne = new TypeParColonneType

      val ite = queryForList(SELECT_TYPE_COLUMNS, extract(label)).iterator
      while(ite.hasNext){
        val entree = ite.next.asInstanceOf[JavaMap[String, String]]
        typesColonne += entree.get("COLNAME") -> entree.get("TYPENAME")
      }
      dicColonne += label -> typesColonne
      typesColonne
    }

    /**
     * Recherche les colonnes formant la clé primaire d'une table et garde en cache le résultat
     */
    def memoPK[A](label:A, extract:A => Array[Object], dicColonnePk:HashMap[A,List[String]]):List[String] = {
      val colonnes = queryForList(SELECT_COLUMNPK, extract(label), classOf[String])
      if (colonnes.isEmpty){
        throw new Exception("pk introuvable")
      }

      var resultat = List[String]()
      val ite = colonnes.iterator
      while(ite.hasNext){
        resultat = ite.next.asInstanceOf[String] :: resultat
      }
      dicColonnePk += label -> resultat
      resultat
    }
  }
}