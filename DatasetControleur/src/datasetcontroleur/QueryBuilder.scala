package datasetcontroleur

import collection.mutable._
import TableIntrospection.TypeParColonneType

object QueryBuilder {

  def buildInsertQuery(tableName:String, attributes:Iterable[(_,_)], typesColonne:TypeParColonneType):String = {
    return "INSERT INTO "+tableName+"(" +  ((attributes.elements.map {x:(_,_) => x._1}).mkString(", ")
                                            + ") VALUES ("
                                            + (attributes.elements.map {x:(_,_) => quote(x._2.toString, x._1.toString, typesColonne)}).mkString(", ") + ")")
  }

  /**
   * Entoure la valeur de quote suivant sa valeur en base
   */
  private def quote[A,B](valeur:String, cleColonne:A, typesColonne:HashMap[A, B]):String = {
    def quote(v:String):String = "'"+v+"'"
    val typeColonne = typesColonne.get(cleColonne).getOrElse("UNKNOWN")
    return typeColonne match{
      case "UNKNOWN" => throw new Exception("----colonne non trouvée pour la clé "+cleColonne)
      case "SMALLINT" | "BIGINT" | "INTEGER" | "DECIMAL"  => valeur
      case "TIMESTAMP" => if (valeur.size < 11){ "'"+valeur+" 00:00:00.0'" }else{quote(valeur)}
      case _ => quote(valeur)
    }
  }
  
}
