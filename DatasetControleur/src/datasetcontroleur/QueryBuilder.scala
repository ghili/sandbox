package datasetcontroleur

import collection.mutable._
import org.apache.commons.lang._
import TableIntrospection.TypeParColonneType

class QueryBuilder {

  def buildInsertQuery(tableName:String, attributes:Iterable[(_,_)], typesColonne:TypeParColonneType):String = {
    "INSERT INTO "+tableName+"(" + ((attributes.elements.map {(_:(_,_))._1}).mkString(", ")
                                 + ") VALUES ("
                                 + (attributes.elements.map {x:(_,_) => format(x._2, x._1.toString, typesColonne)}).mkString(", ") + ")")
  }

  def buildDeleteQuery(tableName:String, attributes:Iterable[(_,_)], colonnesPks:List[String], typesColonne:TypeParColonneType):String = {
    // Récupération des attributs XML à garder dans la clause where du delete, c.a.d ceux correspondant aux colonnes formant la pk de la table
    val pkAttributes = attributes filter {colonnesPks contains (_:(_,_))._1}
    "DELETE FROM "+tableName+" WHERE " + ((pkAttributes.elements.map {x:(_,_) => x._1 +" = "+ format(x._2, x._1.toString, typesColonne)}) mkString " AND ")
  }

  /**
   * formatte la valeur en fonction du type de la colonne
   */
  private def format[A,B,C](paramValeur:A, cleColonne:B, typesColonne:HashMap[B, C]):String = {
    def quote = "'"+ StringEscapeUtils.escapeSql(_:String) +"'"
    val valeur = paramValeur.toString
    val typeColonne = typesColonne get cleColonne getOrElse (throw new Exception("!! Colonne non trouvée pour la clé [" + cleColonne + "] !!"))
    typeColonne match{
      case "SMALLINT" | "BIGINT" | "INTEGER" | "DECIMAL" => valeur
      case "BLOB" => "BLOB(" + quote(valeur) + ")"
      case "TIMESTAMP" => quote(if (valeur.size < 11){ valeur + " 00:00:00.0" }else{valeur})
      case _ => quote(valeur)
    }
  }
}
