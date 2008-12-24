package ctf.gui

import ui._


object AppRecherche  {

    def main(args : Array[String]) = {
        val controller = new UIController
        val view = new CtfView(controller)
        controller.browserAction = new BrowserAction(view)
        controller.finderAction = new FinderAction(view)


        java.awt.EventQueue.invokeLater(new Runnable() {
                def run() {
                    view.setVisible(true)
                }
            })
    }

}