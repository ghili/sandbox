
package ctf.gui.domain

import java.sql.Timestamp
import scala.reflect.BeanInfo

@BeanInfo
case class Fichier {

    var idFichier:Long= _

    var nom:String= _

    var extension:String= _

    var taille:Long= _

    var dateFichier:Timestamp= _

    var dossier:Dossier= _
}
