package tab_files;

import javax.swing.UIManager;
import java.awt.*;

/**
 * <p>Title: GUImaint for CODIAC</p>
 * <p>Description: MySQL db version</p>
 * <p>Copyright: Copyright (c) 2004, 2005, 2006, 2007</p>
 * <p>Company: NCAR/EOL</p>
 * @author Don Stott
 * @version 2.5- test
 */

public class maint {
  boolean packFrame = false;

  //Construct the application
  public maint() {
    Frame1 frame = new Frame1();

    frame.setModule(new DataModule1());

    //Validate frames that have preset sizes
    //Pack frames that have useful preferred size info, e.g. from their layout
    if (packFrame)
      frame.pack();
    else
      frame.validate();
    //Center the window
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    Dimension frameSize = frame.getSize();
    if (frameSize.height > screenSize.height)
      frameSize.height = screenSize.height;
    if (frameSize.width > screenSize.width)
      frameSize.width = screenSize.width;
    frame.setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2 - 15); // subtracting 15 puts it a little higher up
    frame.setVisible(true);
  }

  //Main method
  public static void main(String[] args) {
    try  {
      UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
//      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
//      UIManager.setLookAndFeel(new javax.swing.plaf.metal.MetalLookAndFeel());
    }
    catch(Exception e) {
    }
    new maint();
  }
}
