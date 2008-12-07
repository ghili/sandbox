package ctf.gui;

import java.awt._
import javax.swing._

object App extends JFrame {

    val entry = new JTextField(10)
    val rlistModel = new DefaultListModel()
    val rlist = new JList(rlistModel)

    def main(args : Array[String]) = {
        setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )
        getContentPane.setLayout(new BoxLayout(getContentPane, BoxLayout.Y_AXIS))

        val inputPane = new JPanel
        val inputLayout = new BoxLayout(inputPane, BoxLayout.X_AXIS)
        inputPane.add(new JLabel( "entree:" ))
        inputPane.add(entry)

        val resultPane = new JPanel
        val resultLayout = new BoxLayout(resultPane, BoxLayout.Y_AXIS)
        val listScroller = new JScrollPane(rlist);
        listScroller.setPreferredSize(new Dimension(250, 80))
        resultPane.add(listScroller)

        getContentPane.add(inputPane, BorderLayout.NORTH)
        getContentPane.add(resultPane, BorderLayout.SOUTH)

        pack()
        setVisible( true )
    }
}
