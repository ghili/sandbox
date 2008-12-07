
package ctf.gui.domain

import java.sql.Timestamp
import scala.reflect.BeanProperty

case class Fichier {

    @BeanProperty
    var idFichier:Long= _

    @BeanProperty
    var nom:String= _

    @BeanProperty
    var extension:String= _

    @BeanProperty
    var taille:Long= _

    @BeanProperty
    var dateFichier:Timestamp= _

    @BeanProperty
    var dossier:Dossier= _
}
