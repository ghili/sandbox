package datasetcontroleur

import collection.mutable._
import org.springframework.jdbc.core._
import java.util.{List=> JavaList}
import java.util.{Map=> JavaMap}

 
class Db2TableIntrospection requires(Db2TableIntrospection with DbTemplate) extends TableIntrospection {
  import TableIntrospection._
  
  val SELECT_TYPE_COLUMNS = "SELECT colName, typeName FROM SYSCAT.COLUMNS WHERE tabSchema = ? AND tabName = ?"
  val SELECT_COLUMNPK = """ SELECT kc.colName FROM SYSCAT.KEYCOLUSE kc
                            JOIN SYSCAT.TABCONST cst ON cst.constName = kc.constName AND cst.tabSchema = kc.tabSchema AND cst.tabName = kc.tabName
                            WHERE kc.tabSchema = ? AND kc.tabName = ? AND cst.type = 'P'"""
  
  /**
   *
   */
  def memoColonnes[A](label:A, extract:A => Array[Object],  dicColonne:HashMap[A,TypeParColonneType]):TypeParColonneType = {
    val colonnes:JavaList[_] = queryForList(SELECT_TYPE_COLUMNS, extract(label))
    val typesColonne = new TypeParColonneType

    val ite = colonnes.iterator
    while(ite.hasNext){
      val entree = ite.next.asInstanceOf[JavaMap[String, String]]
      typesColonne += entree.get("COLNAME") -> entree.get("TYPENAME")
    }
    dicColonne += label -> typesColonne
    return typesColonne
  }

  /**
   * Recherche les colonnes formant la clé primaire d'une table et garde en cache le résultat
   */
  def memoPK[A](label:A, extract:A => Array[Object], dicColonnePk:HashMap[A,List[String]]):List[String] = {
    val colonnes:JavaList[_] = queryForList(SELECT_COLUMNPK, extract(label), classOf[String])
    var resultat = List[String]()
    
    if (colonnes.isEmpty){
      throw new Exception("pk introuvable")
    }

    val ite = colonnes.iterator
    while(ite.hasNext){
      val entree = ite.next.asInstanceOf[String]
      resultat = entree :: resultat
    }
    dicColonnePk += label -> resultat
    return resultat
  }
}