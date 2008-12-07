
package ctf.gui.domain

import scala.reflect.BeanProperty

class Dossier {

    @BeanProperty
    var idDossier:Long= _

    @BeanProperty
    var nom:String= _

    @BeanProperty
    var chemin:String= _

    @BeanProperty
    var dossierParent:Dossier= _

    @BeanProperty
    var support:Support= _

}
