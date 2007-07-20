package net.sf.yacas;


import java.lang.*;
import java.awt.*;
import java.applet.*;
import java.awt.event.*;
import java.util.*;
import java.math.*;

public class calc extends Applet implements ActionListener //, Runnable
{
  YacasNotebookComponent notebook;
 
  Button button1 = new Button("1 (./?)");
  Button button2 = new Button("2 (abc)");
  Button button3 = new Button("3 (def)");
  Button button4 = new Button("4 (ghi)");
  Button button5 = new Button("5 (jkl)");
  Button button6 = new Button("6 (mno)");
  Button button7 = new Button("7 (pqrs)");
  Button button8 = new Button("8 (tuv)");
  Button button9 = new Button("9 (wxyz)");
  Button buttonRoll = new Button("*+");
  Button button0 = new Button("0 [ ]");
  Button buttonEnter = new Button("#");

  // arrows
  Button buttonUp       = new Button("/\\");
  Button buttonDown     = new Button("\\/");
  Button buttonLeft     = new Button("<");
  Button buttonRight    = new Button(">");
  Button buttonCenter   = new Button("O");
  Button buttonClear    = new Button("C");

  /* This the only method that is called explicitly -- every other method is
   * called depending on the user's actions.
   */
  public void init()
  {
    notebook = new YacasNotebookComponent(getDocumentBase().toString()+".hints",this);

    //Allows for configuring a layout with the restraints of a grid or
    //something similar
    setLayout(null);
 
    //This will resize the applet to the width and height provided
    resize(276/*420*/,540/*368*/);
 
    //This sets the default font to Helvetica, plain, size 12
    setFont(new Font("Helvetica", Font.PLAIN, 12));

    /* Display Panel, which appears at the top of the screen. The label is
     * placed and sized with the setBounds(x,y,width,height) method, and the
     * font, foreground color and background color are all set. Then the
     * label is added to the layout of the applet.
     */
    notebook.setBounds(42,15,192/*308*/,240);
//    notebook.setFont(new Font("Helvetica", Font.PLAIN, 20));
//    notebook.setForeground(new Color(65280));
//    notebook.setBackground(new Color(0));
//    add(notebook);
 
    /* Memory Panel, which appears just to the right of the Display Panel.
     * The label is placed and sized with the setBounds(x,y,width,height)
     * method, and the font, foreground color and background color are all
     * set. Then the label is added to the layout of the applet.
     */
    /* The following declarations initialize all of the Numberic Buttons
     * that will displayed on the applet.
     *
     * First, an ActionListener (which will capture events) is added to the
     * button, sending the applet as the argument
     *
     * Second, the button is placed and sized with the setBounds(x,y,width,
     * height) method.
     *
     * Then the default font is set for the button, and the button is added
     * to the layout of the applet.
     */
    Font font = new Font("Dialog", Font.BOLD, 9);

    buttonUp.addActionListener(this);
    buttonUp.setBounds(106,274+38*0,60,34);
    buttonUp.setFont(font);
    buttonUp.setForeground(new Color(0xff));
    add(buttonUp);

    buttonLeft.addActionListener(this);
    buttonLeft.setBounds(42,274+38*1,60,34);
    buttonLeft.setFont(font);
    buttonLeft.setForeground(new Color(0xff));
    add(buttonLeft);

    buttonCenter.addActionListener(this);
    buttonCenter.setBounds(106,274+38*1,60,34);
    buttonCenter.setFont(font);
    add(buttonCenter);

    buttonRight.addActionListener(this);
    buttonRight.setBounds(170,274+38*1,60,34);
    buttonRight.setFont(font);
    buttonRight.setForeground(new Color(0xff));
    add(buttonRight);

    buttonDown.addActionListener(this);
    buttonDown.setBounds(106,274+38*2,60,34);
    buttonDown.setFont(font);
    buttonDown.setForeground(new Color(0xff));
    add(buttonDown);

    buttonClear.addActionListener(this);
    buttonClear.setBounds(170,274+38*2,60,34);
    buttonClear.setFont(font);
    buttonClear.setForeground(new Color(0xff0000));
    add(buttonClear);


    button1.addActionListener(this);
    button1.setBounds(42,274+38*3,60,34);
    button1.setFont(font);
    add(button1);

    button2.addActionListener(this);
    button2.setBounds(106,274+38*3,60,34);
    button2.setFont(font);
    add(button2);

    button3.addActionListener(this);
    button3.setBounds(170,274+38*3,60,34);
    button3.setFont(font);
    add(button3);

    button4.addActionListener(this);
    button4.setBounds(42,274+38*4,60,34);
    button4.setFont(font);
    add(button4);

    button5.addActionListener(this);
    button5.setBounds(106,274+38*4,60,34);
    button5.setFont(font);
    add(button5);

    button6.addActionListener(this);
    button6.setBounds(170,274+38*4,60,34);
    button6.setFont(font);
    add(button6);

    button7.addActionListener(this);
    button7.setBounds(42,274+38*5,60,34);
    button7.setFont(font);
    add(button7);

    button8.addActionListener(this);
    button8.setBounds(106,274+38*5,60,34);
    button8.setFont(font);
    add(button8);

    button9.addActionListener(this);
    button9.setBounds(170,274+38*5,60,34);
    button9.setFont(font);
    add(button9);


    buttonRoll.addActionListener(this);
    buttonRoll.setBounds(42,274+38*6,60,34);
    buttonRoll.setFont(font);
    add(buttonRoll);

    button0.addActionListener(this);
    button0.setBounds(106,274+38*6,60,34);
    button0.setFont(font);
    add(button0);

    buttonEnter.addActionListener(this);
    buttonEnter.setBounds(170,274+38*6,60,34);
    buttonEnter.setFont(font);
    add(buttonEnter);
    invalidate();
  }  //end of calc init method
 

 
 
/***************************************************************************
 *
 * This is the overridden update method for this applet -- update is used to
 * eliminate the flicker that happens with repainting an image from scratch.
 *
 **************************************************************************/
  public void update(Graphics g)
  {
    notebook.paint(g);
  }
  public void paint(Graphics g)
  {
    notebook.paint(g);
  }
 
  /************************************************************************
  /*
  /* This method is called whenever an action is performed on the applet,
  /* specifically whenever one of the buttons on the applet is pressed
  /*
  /************************************************************************/
  public void actionPerformed(ActionEvent event)
  {
    Object src = event.getSource();
    if (src instanceof Button)
    {
      if (src == button1) notebook.button_symbol();
      if (src == button2) notebook.button_0('2');
      if (src == button3) notebook.button_0('3');
      if (src == button4) notebook.button_0('4');
      if (src == button5) notebook.button_0('5');
      if (src == button6) notebook.button_0('6');
      if (src == button7) notebook.button_0('7');
      if (src == button8) notebook.button_0('8');
      if (src == button9) notebook.button_0('9');
      if (src == button0) notebook.button_space();

      if (src == buttonRoll) notebook.button_nextword();
      if (src == buttonEnter) notebook.button_enter();
      if (src == buttonUp) notebook.button_up();
      if (src == buttonDown) notebook.button_down();
      if (src == buttonLeft) notebook.button_left();
      if (src == buttonRight) notebook.button_right();
      if (src == buttonCenter) notebook.button_center();
      if (src == buttonClear) notebook.button_clear();
      if (notebook.dirty)
        repaint();
     }
  }//end of actionPerformed method

 
/****************************************************************************
 *
 * The following methods are overridden for the timer Thread, as this applet
 * implements Runnable.
 *
 ****************************************************************************/
/*
  //overridden start method
  public void start()
  {
  }
 
  //overridden stop method
  public void stop()
    {
    }
 
  //overridden run method
  public void run()
  {
  }
*/
}        //end of calc class

