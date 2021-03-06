package datasetcontroleur

import collection.mutable._
import collection.immutable
import org.springframework.jdbc.core._
import java.util.{Map=> JavaMap}

object TableIntrospection {type TypeParColonneType = immutable.Map[String,String]}

trait TableIntrospectionComponent { this: DbTemplateComponent =>
    val tableIntrospection:TableIntrospection
  
    abstract class TableIntrospection {
        import TableIntrospection.TypeParColonneType
        val SELECT_TYPE_COLUMNS:String
        val SELECT_COLUMNPK:String

        /**
         * Recherche les types par colonnes d'une table
         */
        def rechercherTypeColonne(label:String):TypeParColonneType = {
            immutable.Map() ++
            (dbTemplate.queryForList(SELECT_TYPE_COLUMNS, extractSchemaTable(label)) map { entree:JavaMap[String, String] => (entree.get("COLNAME"), entree.get("TYPENAME"))})
        }

        /**
         * Recherche les colonnes formant la clé primaire d'une table et garde en cache le résultat
         */
        def rechercherPk(label:String):List[String] = {
            val resultat = dbTemplate.queryForList(SELECT_COLUMNPK, extractSchemaTable(label), classOf[String])
            if (resultat.isEmpty){
                throw new Exception("pk introuvable")
            }
            resultat
        }
    
        /** sépare le schéma et le nom de la table */
        private def extractSchemaTable(label:String):Array[Object] = (label split '.') map {(_:String).toUpperCase}
    }
}