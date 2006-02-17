package net.sf.yacas;

//
//  tegicphone.java
//  tegicphone
//
//  Created by Ayal Pinkus on Sun Dec 05 2004.
//  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
//  A simple Java applet
//

import java.awt.*;
import java.applet.*;

public class tegicphone extends Applet {

    static final String message = "Hello World!";
    private Font font = new Font("serif", Font.ITALIC + Font.BOLD, 36);
	
    public void init() {
        setLayout (null);
    }
	
    public void paint (Graphics g) {
        g.setColor(Color.blue);
        g.setFont(font);
        g.drawString(message, 40, 80);
    }
}
