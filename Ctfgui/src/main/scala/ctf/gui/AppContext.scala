package ctf.gui

import ui._

class AppContext {
    val sqlMapConfig = new SqlMapConfig("ctf/gui/config/SqlMapConfig.xml")
    val sqlMapper = sqlMapConfig.sqlMapper
    val searcher = new Searcher(this)
    lazy val view:CtfView = new CtfView(this)
    lazy val finderView:FinderView = new FinderView(this)
    lazy val browserView:BrowserView = new BrowserView(this)
    lazy val controller:UIController = new UIController(this)
}
