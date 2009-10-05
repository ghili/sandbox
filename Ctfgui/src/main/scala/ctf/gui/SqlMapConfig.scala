

package ctf.gui

import _root_.com.ibatis.sqlmap.client._
import _root_.com.ibatis.common.resources.Resources
import java.util.{List => JavaList}
import collection.jcl.BufferWrapper

class SqlMapConfig(configPath:String) {
    var sqlMapper:SqlMapClient = _

    try {
        val reader = Resources.getResourceAsReader(configPath)
        sqlMapper = SqlMapClientBuilder.buildSqlMapClient(reader)
        reader.close
    } catch {
        case e:Exception => e.printStackTrace()
    }

}

object CustomConversions {
    implicit def convertList[T](javaList:JavaList[_]):List[T] = new BufferWrapper[T]{def underlying = javaList.asInstanceOf[JavaList[T]]}.toList
}

