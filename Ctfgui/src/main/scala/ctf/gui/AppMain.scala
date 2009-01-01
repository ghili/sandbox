package ctf.gui

import ui._


object AppRecherche  {

    def launch = {
        val ctx = new AppContext

        java.awt.EventQueue.invokeLater(new Runnable() {
                def run() {
                    ctx.view.setVisible(true)
                }
            })
    }

    def main(args : Array[String]) = {
        launch
    }

}