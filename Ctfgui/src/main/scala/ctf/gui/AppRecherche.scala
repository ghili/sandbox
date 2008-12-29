package ctf.gui

import ui._


object AppRecherche  {

    def launch = {
        val controller = new UIController
        val view = new CtfView(controller)
        controller.browserAction = new BrowserAction(view)
        controller.finderView = new FinderView(view)


        java.awt.EventQueue.invokeLater(new Runnable() {
                def run() {
                    view.setVisible(true)
                }
            })
    }

    def main(args : Array[String]) = {
        launch
    }

}