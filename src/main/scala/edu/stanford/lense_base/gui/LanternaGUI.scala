package edu.stanford.lense_base.gui

import com.googlecode.lanterna.TerminalFacade
import com.googlecode.lanterna.terminal.Terminal

/**
 * Created by keenon on 4/29/15.
 *
 * This should be a simple display for in-progress LENSE computations, showing how likelihood guesses, costs, delays,
 * etc are progressing
 */
class LanternaGUI {
  val terminal : Terminal = {
    val t = TerminalFacade.createTerminal()
    t.enterPrivateMode()
    t
  }
}
