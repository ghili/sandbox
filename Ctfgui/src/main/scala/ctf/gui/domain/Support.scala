
package ctf.gui.domain

import scala.reflect.BeanInfo
import java.sql.Timestamp

@BeanInfo
class Support {

    var idSupport:Long = _

    var nom:String= _

    var chck:Long= _

    var dateCreation:Timestamp= _
}
