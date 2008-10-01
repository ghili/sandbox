/*
 * Main.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package datasetcontroleur 
import xml._
import collection.mutable._
import org.springframework.context.support._
import org.springframework.jdbc.core._
import org.springframework.transaction.support._
import org.springframework.transaction._
import org.springframework.jdbc.datasource.DataSourceTransactionManager
import java.util.{List=> JavaList}
import java.util.{Map=> JavaMap}

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) = {
    val context = new ClassPathXmlApplicationContext(Array[String]("config/config.xml"));
    val jdbcTemplate = context.getBean("UtilDao").asInstanceOf[UtilDao].getJdbcTemplate;
    val txManager = context.getBean("txManager").asInstanceOf[DataSourceTransactionManager]
    val txDef = new DefaultTransactionDefinition
    txDef.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW)

    val fileName = args(0)
    val dataset = XML.loadFile(fileName)
    val datas = dataset.child
    
    val txStatus = txManager.getTransaction(txDef)
    val dicoColonne:HashMap[String,HashMap[String,String]] = new HashMap()

    try{
      for(data <- datas if data.label !=  "#PCDATA"){
        val typesColonne:HashMap[String,String] = dicoColonne.get(data.label).getOrElse(memoColonnes(data.label, jdbcTemplate, dicoColonne))
        var requete = "INSERT INTO "+data.label+"("
        val attributes = data.attributes.filter({x:MetaData => x.value.toString != "[NULL]"})
        //val cles = attributes.map({x:MetaData => x.key})
        //val cles = (for (att:MetaData <-data.attributes if att.value.toString != "[NULL]") yield att.key)
        //val valeurs = attributes.map({x:MetaData => quote(x.value.toString, x.key, typesColonne)})
        val pairs = for (att:MetaData <-data.attributes if att.value.toString != "[NULL]") yield (att.key, quote(att.value.toString, att.key, typesColonne))
        //requete+= (cles.mkString(", ") + ") VALUES (" + valeurs.mkString(", ") + ")")
        requete+= ((pairs.elements.map {x:Tuple2[_,_] => x._1}).mkString(", ")
                   + ") VALUES ("
                   + (pairs.elements.map {x:Tuple2[_,_] => x._2}).mkString(", ") + ")")
        println(requete)
        //jdbcTemplate.execute(requete)
      }
    } finally{
      txManager.rollback(txStatus)
    }
    print("cool")
  }

  /**
   * Entoure la valeur de quote suivant sa valeur en base
   */
  def quote[A,B](valeur:String, cleColonne:A, typesColonne:HashMap[A, B]):String = {
    def quote(v:String):String = "'"+v+"'"
    val typeColonne = typesColonne.get(cleColonne).getOrElse("UNKNOWN")
    return typeColonne match{
      case "UNKNOWN" => throw new Exception("----colonne non trouvée pour la clé "+cleColonne)
      case "SMALLINT" | "BIGINT" | "INTEGER" | "DECIMAL"  => valeur
      case "TIMESTAMP" => if (valeur.size < 11){ "'"+valeur+" 00:00:00.0'" }else{quote(valeur)}
      case _ => quote(valeur)
    }
  }

  /**
   *
   */
  def memoColonnes(label:String, jdbcTemplate: JdbcTemplate, dicColonne:HashMap[String,HashMap[String,String]]):HashMap[String,String] = {
    val pos = label.indexOf(".")
    //val tableInfo:Array[String] = label.split(".")
    val schema = label.substring(0,pos)
    val tabName = label.substring(pos+1)
    val colonnes:JavaList[_] = jdbcTemplate.queryForList("SELECT colName, typeName FROM SYSCAT.COLUMNS WHERE tabSchema ='"+schema+"' AND tabName = '" + tabName +"'")
    val typesColonne:HashMap[String, String] = new HashMap

    val ite = colonnes.iterator    
    while(ite.hasNext){
      val entree = ite.next.asInstanceOf[JavaMap[String, String]]
      typesColonne += entree.get("COLNAME") -> entree.get("TYPENAME")
    }
    dicColonne += label -> typesColonne
    return typesColonne
  }
  
  /*def iterateurAuxiliaire[A,B](ite:Iterator[Tuple2[A,B]], f:Tuple2[A,B]=>B):Iterator[B] = new Iterator[B]{
    def hasNext = ite.hasNext
    def next = f(ite.next)
  }*/

}

