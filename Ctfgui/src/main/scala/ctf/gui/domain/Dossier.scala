
package ctf.gui.domain

import scala.reflect.BeanInfo

@BeanInfo
class Dossier {

    var idDossier:Long= _

    var nom:String= _

    var chemin:String= _

    var dossierParent:Dossier= _

    var support:Support= _

}
