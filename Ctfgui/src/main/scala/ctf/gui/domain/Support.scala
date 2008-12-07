
package ctf.gui.domain

import scala.reflect.BeanProperty
import java.sql.Timestamp

class Support {

    @BeanProperty
    var idSupport:Long = _

    @BeanProperty
    var nom:String= _

    @BeanProperty
    var chck:Long= _

    @BeanProperty
    var dateCreation:Timestamp= _
}
