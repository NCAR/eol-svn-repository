/**6/15/11- Monica Jacobs
 * 
 * This is a mock-up GUI for the new XQC software. Some minor functionality has been
 *  implemented or half-implemented (such as the action listeners on some of the buttons)
 */
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

/** This is a mock-up of the GUI for the new XQC software */
public class gui {

	JFrame frame = new JFrame();
	JPanel skewT = new JPanel();
	JPanel graph = new JPanel();
	JPanel controls = new JPanel();
	JPanel top = new JPanel();
	JPanel bottom = new JPanel();


	//TODO: figure out how to draw rectangular placeholders, or change the size of the JPanels or something

	JButton clear = new JButton("Clear");
	JButton next = new JButton("Next");
	JButton prev = new JButton("Previous");
	JButton save = new JButton("Save");
	JButton quit = new JButton("Quit");

	JButton hi = new JButton("Highlight Interpolated Data");
	JButton zoom = new JButton("Zoom");

	JLabel siteN = new JLabel("Enter First 3 Letters of Site Name:");
	JTextField site = new JTextField(3);

	JButton saveI = new JButton("Save Image");
	JButton read = new JButton("Read");

	Object[] markAsOpt = {"Good", "Bad", "Questionable"};
	JComboBox markAs = new JComboBox(markAsOpt);

	JCheckBox pres = new JCheckBox("Pressure");
	JCheckBox dp = new JCheckBox("Dew Point");
	JCheckBox temp = new JCheckBox("Temperature");
	JCheckBox winds = new JCheckBox("Winds");
	JLabel pressure = new JLabel("Pressure:  ");

	JFileChooser saveFC = new JFileChooser();
	JFileChooser saveIFC = new JFileChooser();
	JFileChooser readFC = new JFileChooser();

	public void initGUI(){		
		//Initializes frame settings
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setSize(1200,1000);
		frame.setTitle("XQC Software Mock-up");
		frame.setResizable(false);
		frame.setLayout(new BorderLayout());

		try {
		    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
		    System.out.println("Exception!");
		}
		
		controlsInit();

		//Adds JPanels to frame
		frame.add(skewT,BorderLayout.WEST);
		frame.add(graph,BorderLayout.CENTER);
		frame.add(controls,BorderLayout.EAST);
		frame.add(top, BorderLayout.NORTH);
		frame.add(bottom,BorderLayout.SOUTH);
		graph.setBackground(Color.white);

		top.setLayout(new BoxLayout(top,BoxLayout.Y_AXIS));
		top.add(new JLabel(" "));
		top.add(new JLabel(" "));
		top.add(new JLabel(" "));
		top.add(new JLabel(" "));
		top.add(new JLabel(" "));

		bottom.setLayout(new BoxLayout(bottom,BoxLayout.Y_AXIS));
		bottom.add(new JLabel(" "));
		bottom.add(new JLabel(" "));
		bottom.add(new JLabel(" "));
		bottom.add(new JLabel(" "));
		bottom.add(new JLabel(" "));
		bottom.add(new JLabel(" "));
		bottom.add(new JLabel(" "));

		frame.setVisible(true);
	}

	private void controlsInit(){
		controls.setLayout(new BorderLayout());

		JPanel topButtons = new JPanel();
		JPanel tb1 = new JPanel();
		JPanel tb2 = new JPanel();
		JPanel tb3 = new JPanel();
		topButtons.setLayout(new BorderLayout());
		tb1.setLayout(new BoxLayout(tb1, BoxLayout.Y_AXIS));
		tb2.setLayout(new BoxLayout(tb2, BoxLayout.Y_AXIS));
		tb3.setLayout(new BoxLayout(tb3, BoxLayout.Y_AXIS));
		tb1.add(clear);
		tb1.add(new JLabel(" "));
		tb1.add(next);
		tb1.add(new JLabel(" "));
		tb1.add(prev);
		tb2.add(new JLabel(" "));
		tb2.add(save);
		tb2.add(new JLabel(" "));
		tb2.add(quit);
		tb3.add(new JLabel(" "));
		tb3.add(hi);
		tb3.add(new JLabel(" "));
		tb3.add(zoom);
		topButtons.add(tb1,BorderLayout.NORTH);
		topButtons.add(tb2,BorderLayout.CENTER);
		topButtons.add(tb3,BorderLayout.SOUTH);

		JPanel empty = new JPanel();
		JPanel midOps = new JPanel();
		JPanel mo1 = new JPanel();
		JPanel mo2 = new JPanel();
		midOps.setLayout(new BorderLayout());
		mo1.setLayout(new FlowLayout());
		mo2.setLayout(new BoxLayout(mo2, BoxLayout.Y_AXIS));
		mo1.add(siteN);
		mo1.add(site);
		mo2.add(saveI);
		mo2.add(new JLabel(" "));
		mo2.add(read);
		mo2.add(new JLabel(" "));
		mo2.add(new JLabel(" "));
		empty.setLayout(new BoxLayout(empty, BoxLayout.Y_AXIS));
		empty.add(new JLabel(" "));
		empty.add(new JLabel(" "));


		midOps.add(empty, BorderLayout.NORTH);
		midOps.add(mo1,BorderLayout.CENTER);
		midOps.add(mo2,BorderLayout.SOUTH);

		JPanel bottomOpts = new JPanel();
		JPanel bo1 = new JPanel();
		JPanel bo1a = new JPanel();
		bo1a.setLayout(new BoxLayout(bo1a, BoxLayout.Y_AXIS));
		bottomOpts.setLayout(new BorderLayout());
		bo1.setLayout(new BoxLayout(bo1, BoxLayout.Y_AXIS));
		bo1.add(new JLabel(" "));
		bo1.add(pres);
		bo1.add(dp);
		bo1.add(temp);
		bo1.add(winds);
		bo1.add(new JLabel(" "));
		bo1a.add(pressure);
		bo1a.add(new JLabel(" "));
		bo1a.add(new JLabel(" "));
		bo1a.add(new JLabel(" "));
		bo1a.add(new JLabel(" "));
		bo1a.add(new JLabel(" "));
		bo1a.add(new JLabel(" "));
		bo1a.add(new JLabel(" "));
		bo1a.add(new JLabel(" "));

		bottomOpts.add(markAs, BorderLayout.NORTH);
		bottomOpts.add(bo1,BorderLayout.CENTER);
		bottomOpts.add(bo1a,BorderLayout.SOUTH);

		controls.add(topButtons,BorderLayout.NORTH);
		controls.add(midOps, BorderLayout.CENTER);
		controls.add(bottomOpts, BorderLayout.SOUTH);

		setActions();
	}
	
	
	private void setActions(){

		save.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				int returnVal = saveFC.showSaveDialog(null);
				
			}});
		saveI.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				int returnVal = saveIFC.showSaveDialog(null);
			}});
		read.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				int returnVal = readFC.showOpenDialog(null);
			}});
		quit.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				frame.setVisible(false);
				frame.dispose();
				System.exit(-1);
			}});
		site.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				String siteName =site.getText();
				if (siteName.length() >=3){
					System.out.println("First 3 letters: "+siteName.substring(0,3).toLowerCase());
				}
				else{
					System.out.println(siteName.toLowerCase());
				}
			}});
		clear.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				frame.repaint();
			}});

	}

	
	//TODO: Could create a class graph which is paintable and also can have action listeners?

	
	public static void main(String args[]){
		gui GUI = new gui();
		GUI.initGUI();
	}
}
