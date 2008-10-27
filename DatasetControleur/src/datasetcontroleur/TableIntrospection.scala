package datasetcontroleur

import collection.mutable._
import org.springframework.jdbc.core._
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
    def memoColonnes(label:String, dicColonne:HashMap[String,TypeParColonneType]):TypeParColonneType = {
      val typesColonne = new TypeParColonneType

      for(entree:JavaMap[String, String]<- queryForList(SELECT_TYPE_COLUMNS, extractSchemaTable(label))){
        typesColonne += entree.get("COLNAME") -> entree.get("TYPENAME")
      }
      dicColonne += label -> typesColonne
      typesColonne
    }

    /**
     * Recherche les colonnes formant la clé primaire d'une table et garde en cache le résultat
     */
    def memoPK(label:String, dicColonnePk:HashMap[String,List[String]]):List[String] = {
      val resultat = queryForList(SELECT_COLUMNPK, extractSchemaTable(label), classOf[String])
      if (resultat.isEmpty){
        throw new Exception("pk introuvable")
      }
      
      dicColonnePk += label -> resultat
      resultat
    }
    
    /** sépare le schéma et le nom de la table */
    private def extractSchemaTable(label:String):Array[Object] = (label split '.') map {(_:String).toUpperCase}
  }
}