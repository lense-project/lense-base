package edu.stanford.lense_base.gui

import java.awt.EventQueue
import java.awt.Graphics
import java.awt.Graphics2D
import javax.swing.JFrame
import javax.swing.JPanel

import edu.stanford.lense_base.LenseEngine

/**
 * Created by keenon on 4/29/15.
 *
 * It might be a pain in the butt to render arbitrary graphs in Lanterna, and actually have them fit. Will probably
 * always have access to a window manager when developing this anyways, so trying something more ambitious seems
 * somewhat appropriate
 */
class Java2DGUI(lense : LenseEngine) {
  class Surface extends JPanel {
    def doDrawing(g : Graphics) = {
      val g2d : Graphics2D = g.asInstanceOf[Graphics2D]
      g2d.drawString("Java 2D", 50, 50)
      println("Drawing")
    }

    override def paintComponent(g : Graphics) = {
      super.paintComponent(g)
      doDrawing(g)
    }

    // lense.
    // repaint()
  }

  class MainFrame extends JFrame {
    add(new Surface())
    setTitle("Simple Java 2D example")
    setSize(300, 200)
    setLocationRelativeTo(null)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)


  }

  EventQueue.invokeLater(new Runnable {
    def run(): Unit = {
      val ex = new MainFrame()
      ex.setVisible(true)
    }
  })
}
