

package ctf.gui

import com.ibatis.sqlmap.client._
import com.ibatis.common.resources.Resources

object SqlMapConfig {
  var sqlMapper:SqlMapClient = _


    try {
        val reader = Resources.getResourceAsReader ("ctf/gui/config/SqlMapConfig.xml")
        sqlMapper = SqlMapClientBuilder.buildSqlMapClient(reader)
        reader.close
     } catch { case e:Exception => e.printStackTrace()
     }
}

