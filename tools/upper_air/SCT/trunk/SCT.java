/** @ Monica Jacobs- July 2011

 SCT.java- This java program starts a GUI which allows a user to select multiple datasets and plot them on five plots. X11 forwarding must be enabled in order for this software to work. sct_gnuTemplate.gnu, SCT_DataFilter.java and sct_quit.sh must also be included in the working directory in order for this software to run properly. This software requires java, gnuplot and /bin/bash.

Execute: SoundingGraph.java

There are no input parameters.

Output:
	In the course of running this software, it creates two gnuplot script files: 'file1.gnu' and 'file2.gnu'.
 It also creates the five small plots as gifs ('tempPlot.gif', 'rhPlot.gif', 'windSPlot.gif', 'windDPlot.gif', 
'arPlot.gif'), as well as the five larger plots ('z_tempPlot.gif', 'z_rhPlot.gif', 'z_windSPlot.gif',
'z_windDPlot.gir', 'z_arPlot.gif'). These plots are made from up to five rewritten datasets, which are placed in
 the current directory. However, all of these files are removed when either the clear or quit button is pressed.


This software is run assuming that the data files selected are sounding files and their datatables are organized according to the NCAR EOL Sounding Composite (ESC) format. Additionally, the files opened SHOULD NOT BE IN THE SAME DIRECTORY AS THIS FILE. This program also operates under the assumption that an opened file is not empty or incomplete. In these situations, the plots will not be generated.

*/


import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.FlowLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.Scanner;
import javax.imageio.*;

import javax.swing.*;
import java.awt.*;


public class SCT {


	JFrame frame = new JFrame();
	
	JPanel p = new JPanel();

	static //variable/component initialization
	File[] files = new File[5];

	File template = new File("sct_gnuTemplate.gnu");

	JTextArea header1 = new JTextArea();
	JTextArea header2 = new JTextArea();
	JTextArea header3 = new JTextArea();
	JTextArea header4 = new JTextArea();
	JTextArea header5 = new JTextArea();
	
	JTextArea[] headers = {header1,header2,header3,header4,header5};

	String[] varNames = {"Temperature", "Relative Humidity", "Wind Speed", "Wind Direction", "Ascension Rate","Longitude", "Latitude", "U Wind","V Wind"};
	int[] varNums={3, 5, 8, 9, 10, 11, 12, 6, 7};
	File[] imageFiles={new File("tempPlot.gif"),new File("rhPlot.gif"),new File("windSPlot.gif"),
new File("windDPlot.gif"),new File("arPlot.gif"),new File("lonPlot.gif"),new File("latPlot.gif"),new File("uwindPlot.gif"),new File("vwindPlot.gif")};
	File[] rewriteFiles = new File[9];

	String[] varPane1={"Temperature", "Relative Humidity", "Wind Speed", "Wind Direction", "Ascension Rate"};
	String[] varPane2={"Longitude", "Latitude", "U Wind", "V Wind"};

	int wdcount=1;
	boolean pane=true;

	JFrame errorBox = new JFrame();

	JPanel leftSide = new JPanel();
	JPanel graph = new JPanel();
	JPanel controls = new JPanel();
	JPanel top = new JPanel();
	JPanel bottom = new JPanel();
	JPanel topLeft = new JPanel();
	JPanel topCenter = new JPanel();
	JPanel topRight = new JPanel();

	JButton swap = new JButton("More Plots");
	JButton clear = new JButton("Clear");
	JButton quit = new JButton("Quit");
	JButton plot = new JButton("Plot");
	JButton open = new JButton("Open");

	JButton zoomTest = new JButton("Zoom");
	JComboBox zoomBox = new JComboBox(varNames);

	String[] yvars = {"Pressure", "Time (Upsonde)", "Time (Dropsonde)", "Altitude"};
	int[] yvarNums={2,1,1,15};
	JComboBox yaxis = new JComboBox(yvars); 
	int yAxisSelection = 0;
	JLabel ylabel = new JLabel("Y Variable");

	JFileChooser readFC = new JFileChooser();
	
	JLabel plot1=new JLabel();
	JLabel plot2=new JLabel();
	JLabel plot3=new JLabel();
	JLabel plot4=new JLabel();
	JLabel plot5=new JLabel();

	JLabel instruct = new JLabel("Please select a file");

	int zoomVal=0;

	double yPresMax=0;
	double yTimeMax=0;
	double yPresMin=-1.0;

/**
Returns the first two lines of the header from File f
f is the input file being read
*/
public String getHeader(File f){
		String head="";
		String line = "";
	
		try{
			Scanner s = new Scanner(f);

			int j=0;
			while (s.hasNextLine() && j<2){
				j++;
				line = s.nextLine();
				head=head + "\n" + line+"  ";
			}}

		catch (FileNotFoundException e){
			displayError("FileNotFoundException","Please select a valid file");
		}
		
		return head;
	}
	

/** This method sets the size of the headers*/
	private void setHeaders(){
	

                int headWidth = (frame.getSize().width-250)/5;

                //Initializess header settings
                header1.setPreferredSize(new Dimension(headWidth,100));
                header1.setLineWrap(true);
                header2.setPreferredSize(new Dimension(headWidth,100));
                header2.setLineWrap(true);
                header3.setPreferredSize(new Dimension(headWidth,100));
                header3.setLineWrap(true);
                header4.setPreferredSize(new Dimension(headWidth,100));
                header4.setLineWrap(true);
                header5.setPreferredSize(new Dimension(headWidth,100));
                header5.setLineWrap(true);
	}	

	/** 
	 * This method sets the gui initial values and displays it
	 */
	public void initGUI(){		

		//Initializes frame settings
		frame.setSize(1500,1000);
		frame.setTitle("Sounding Comparison Software");
		frame.setLayout(new BorderLayout());
		frame.setVisible(true);
		frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
	
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {//This is a trivial internal error which should not involve the user
		}

		leftSide.setBackground(Color.black);
	
		setHeaders();		

		header1.setEditable(false);
		header2.setEditable(false);
		header3.setEditable(false);
		header4.setEditable(false);
		header5.setEditable(false);		

		header1.setFont(new Font("SansSerif", 1, 12));
		header2.setFont(new Font("SansSerif",1,12));
		header3.setFont(new Font("SansSerif",1,12));
		header4.setFont(new Font("SansSerif",1,12));
		header5.setFont(new Font("SansSerif",1,12));

		header1.setForeground(new Color(255,0,75));
		header2.setForeground(new Color(34,239,84));
		header3.setForeground(new Color(20,144,255));
		header4.setForeground(new Color(221,160,255));
		header5.setForeground(Color.cyan);

		header1.setBackground(Color.black);
		header2.setBackground(Color.black);
		header3.setBackground(Color.black);
		header4.setBackground(Color.black);
		header5.setBackground(Color.black);

		//Adds JPanels to frame
		frame.add(leftSide,BorderLayout.WEST);
		frame.add(graph,BorderLayout.CENTER);
		frame.add(controls,BorderLayout.EAST);
		frame.add(top, BorderLayout.NORTH);
		frame.add(bottom,BorderLayout.SOUTH);

		top.setLayout(new BorderLayout());
		top.add(topLeft, BorderLayout.WEST);
		top.add(topCenter, BorderLayout.CENTER);
		top.add(topRight, BorderLayout.EAST);

		bottom.setLayout(new BoxLayout(bottom,BoxLayout.Y_AXIS));
		bottom.setBackground(Color.black);
		bottom.add(new JLabel(" "));
		bottom.add(new JLabel(" "));
	
		ylabel.setForeground(Color.white);

		controlsInit();
		
		frame.setVisible(true);
	
		if (pane){
			setZoomBox(varPane1);
		}

		graph.setLayout(new FlowLayout());
		
	}


	/** 
	 * This method formats and displays all the control buttons
	 */
	private void controlsInit(){
		controls.setLayout(new BorderLayout());
		controls.setBackground(Color.white);

		//creates and formats JPanels
		JPanel topButtons = new JPanel();
		topButtons.setLayout(new BoxLayout(topButtons, BoxLayout.Y_AXIS));
		topButtons.setBackground(Color.white);
		
		//adds top JButtons		
		topLeft.setLayout(new BoxLayout(topLeft, BoxLayout.X_AXIS));
		topRight.setLayout(new BoxLayout(topRight, BoxLayout.Y_AXIS));
		
		top.add(topLeft, BorderLayout.WEST);
		top.add(topCenter, BorderLayout.CENTER);
		top.add(topRight, BorderLayout.EAST);
		
		topCenter.setBackground(Color.black);
		topRight.setBackground(Color.black);
		topRight.add(new JLabel(" "));
		topRight.add(quit);
		topRight.add(new JLabel(" "));
		topRight.add(clear);
		clear.setVisible(false);	
		topRight.add(new JLabel(" "));
		
		topRight.add(ylabel);
		ylabel.setVisible(false);
		topRight.add(yaxis);
		topRight.add(plot);
		yaxis.setVisible(false);

		topRight.add(new JLabel(" "));
		topRight.add(zoomBox);
		zoomBox.setVisible(false);
		topRight.add(zoomTest);
		zoomTest.setVisible(false);

		plot.setVisible(false);
		topRight.add(new JLabel(" "));
		topLeft.setBackground(Color.black);
		topLeft.add(headers[0]);
		topLeft.add(headers[1]);
		topLeft.add(headers[2]);
		topLeft.add(headers[3]);
		topLeft.add(headers[4]);
	
		instruct.setFont(new Font("serif",1,24));
		graph.add(instruct);
		
		topLeft.add(open);	
		topRight.add(swap);
		topRight.add(new JLabel(" "));
		swap.setVisible(false);
	
		readFC.setCurrentDirectory(new File("/net/work"));

		controls.add(topButtons,BorderLayout.NORTH);

		setActions();
	}
	

/**
This method sets the items in zoomBox
list is the list of object being displayed in zoomBox
*/
	private void setZoomBox(Object[] list){
		zoomBox.removeAllItems();
		for (int i=0; i<list.length; i++){
			zoomBox.addItem(list[i]);
		}
	}


/** 
This method writes a new file with '?' replacing the current values for missing data
f is original file
index is the array index of the file
y is the column number of the selected y-variable
*/
	private void  writeData(File f,int index,int y){
		String newName=f.getName().substring(0,f.getName().length()-4)+".sct"; //file name without path
	
		//in case of files with the same name
		if(containsFile(newName,rewriteFiles)){
			newName = newName.substring(0,newName.length()-4)+wdcount+".sct";
			wdcount++;
		}		
		
		File newfile = new File(newName);
		boolean header = true;
		int counter=1;
	try{
		FileWriter write = new FileWriter(newfile);
		Scanner scan = new Scanner(f);

		while(scan.hasNextLine()){
			String nl = scan.nextLine();
			Scanner wordScan=new Scanner(nl);
		
			if(header){write.write(nl+"\n");}

			int c=0;
			String newLine="";
			while((!header)&&(wordScan.hasNext())){
				c++;

				//Replaces appropriate values with missing markers
				String word =wordScan.next();
				if(((c==2)||(c==15)||(c==6)||(c==7))&&(word.equals("9999.0"))){
					newLine=newLine + " ?";
					word="?";
				}
				else if((((c!=2)&&(c!=15))&&(word.equals("999.0")))||(word.equals("99999.0"))){
					newLine=newLine+" ?";
					word="?";
				}
				else if(((c==11)&&(word.equals("9999.000")))||((c==12)&&word.equals("999.000"))){
					newLine=newLine+" ?";
					word="?";
				}
				
				else{
					newLine=newLine+" "+word;
				}
			try{	
			//updates the maximum and minimum pressure and maximum time values (for upside-down plotting purposes)
				double wordVal=Double.parseDouble(word);
				if((c==2)&&(wordVal>yPresMax)){
					yPresMax=wordVal;
				}
				else if((c==1)&&(wordVal>yTimeMax)){
					yTimeMax=wordVal;
				}
				else if((c==2)&&((yPresMin<0)||(wordVal<yPresMin))){
					yPresMin=wordVal;
				}
			}
			catch (NumberFormatException ne){//this error is intentionally ignored
			}
			}
				write.write(newLine+"\n");
			if((nl.length()>1) && (nl.charAt(1)=='-')){header=false;}

			}		}
	catch (IOException e){displayError("IOException","Unsuccessful file write. Please try again");}

			rewriteFiles[index]= newfile;
	}

/**
This method returns the number of files in an array
f is the array of files of which the number of files is being counted
return the number of files in f
*/
	private int getLengthFiles(File[] f){
		int count = 0;
		while((count<f.length)&&(f[count]!=null)){count++;}
		return count;
		}

/**
This method runs the file1.gnu as a gnuplot script. If file1.gnu or file2.gnu does not exist, exception is caught
returns if a gif has been created
*/
	private boolean callGnuplot(){

		try{			
		Runtime r = Runtime.getRuntime();
		Process p = r.exec("gnuplot file1.gnu");
		Process p2 = r.exec("gnuplot file2.gnu");
		p2.waitFor();
		}
		catch (Exception e){
		displayError("Exception","Unsuccessful runtime operation");
		}

		try{
		FileInputStream fis = new FileInputStream(imageFiles[0]);
		
		int check = fis.read();
		if (check==-1){
			displayError("Empty or Incomplete Dataset","Empty or Incomplete Dataset Seleced: Please select only vaild files");
			return false;
		}
		}
		catch (IOException ioe){displayError("IOException","Problem reading file");}		

		return true;
		}


/**
This method writes new gnuplot script files 'file1.gnu' and 'file2.gnu'
tempf is the template file being read 
i indicates which plot is being created
yvar is the column number of the y variable being plotted
*/
	private void updateTemplate(File tempf,int i,int yvar){
	
	File file1 = new File("file1.gnu");

	   try{
	
	//creates the gnuplot script to generate the small plots
	FileWriter writer = new FileWriter(file1);
	      Scanner s = new Scanner(tempf);
	
	if(yvar==2){
		writer.write("set yrange["+(yPresMin-100)+":"+(yPresMax+100)+"]reverse\n");
		}
	else if((yvar==1)&&(yAxisSelection==2)){
		writer.write("set yrange[0:"+(yTimeMax+50)+"]reverse\n");
		}

           while (s.hasNextLine()){
 		String nextLine = s.nextLine();	
		String plotFile = "plot \""+rewriteFiles[0]+"\" using "+varNums[i]+":"+yvarNums[yAxisSelection];

		for (int j=1; j<files.length; j++){
			if(rewriteFiles[j]!=null){
				plotFile = plotFile +", \""+rewriteFiles[j]+"\" using "+varNums[i]+":"+yvarNums[yAxisSelection];
			}
		}	

		//rewrites certain likes from the template file with correct variables
		if(nextLine.contains("FILE")){
	
			writer.write(plotFile+"\n");
		}
		else if(nextLine.contains("IMAGE")){
			writer.write("set output \""+imageFiles[i]+"\"\n");
		}
		else if(nextLine.contains("TITLE")){
			writer.write("set title \""+yvars[yAxisSelection] +" vs. "+varNames[i]+"\"\n");
		}
		else if(nextLine.contains("XLABEL")){
			writer.write("set xlabel \""+varNames[i]+"\"\n");
		}
		else if(nextLine.contains("YLABEL")){
			writer.write("set ylabel\""+yvars[yAxisSelection]+"\"\n");
		}
		else if(nextLine.contains("SIZE")){
			double dimX=frame.getSize().width/2260.0;
			double dimY=(frame.getSize().height-250)/1065.0;
			writer.write("set size "+dimX+","+dimY+"\n");
		}
		else{
		    writer.write(nextLine+"\n");
		}

		
	  }
		s.close();
		writer.close();
          }
          catch (FileNotFoundException ex){
             displayError("FileNotFoundException","File Not Found");
          }
	  catch (IOException ioex){
		displayError("IOException","Input Stream Error");
	 }

	try{
		//creates the gnuplot script to generate the zoom plots
		FileWriter writer2 = new FileWriter("file2.gnu");
		Scanner scan = new Scanner(file1);
	
		writer2.write("set key box\n");
		writer2.write("set key out\n");

		while(scan.hasNextLine()){
			String nextLine=scan.nextLine();	

			if (nextLine.contains("size")){
				int zoomHeight=(frame.getSize().height-500)/250;
				writer2.write("set size "+zoomHeight+","+zoomHeight+"\n");

			}
			else if(nextLine.contains("output")){
				writer2.write("set output \"z_"+imageFiles[i]+"\"\n");
			}
			else if(nextLine.contains("nokey")){}
			else{
				writer2.write(nextLine+"\n");		
			}
		}
	
		scan.close();
		writer2.close();
         }

	catch (Exception e){
		displayError("Exception","Error in writing file2.gnu");
	}

}
/** This method creates and initializes the zoom frame
i is the number of the plot to be viewed
*/
	private void initZoomFrame(int i){
		final JFrame zoomFrame = new JFrame("ZOOM");
		JPanel zoomP = new JPanel();
		JButton zoomQuit = new JButton("Exit");	

		zoomFrame.setSize(frame.getSize().width, frame.getSize().height);		

		zoomFrame.setVisible(true);
		
		zoomFrame.add(zoomP);
		zoomP.setLayout(new FlowLayout());
		zoomP.add(zoomQuit);
		zoomP.setBackground(Color.black);
		JLabel zoomPic = new JLabel();
		zoomP.add(zoomPic);
	
		Image zimg=Toolkit.getDefaultToolkit().createImage("z_"+imageFiles[i]);
		zoomPic.setIcon(new ImageIcon(zimg));			

		zoomQuit.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				zoomFrame.dispose();
		}});

	}

	
	/**
	This method finds the location of a string in an array
	s is the string being searched for
	array is the array of strings being searched
	return the index of the string s in array array
	*/
	private int getIndexOf(String s,String[] array){
		for (int i=0; i<array.length; i++){
			if (array[i].equals(s)){
				return i;
			}
		}	
			return 0;
	}


	/** This method determines if a file is contained in a particular array
	f is the name of the file being searched for
	fArr is the array of files being searched
	return a boolean indicating whether or not the file is contained
	*/
	private boolean containsFile(String f, File[] fArr){
		boolean contains =false;
		for (int i=0; i<getLengthFiles(fArr); i++){
			if (fArr[i].getName().equals(f)){
				contains = true;
			}
		}
		return contains;
	}




	/**
	 * Creates action listeners		
	 */
	private void setActions(){

		open.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				readFC.setFileFilter(new SCT_DataFilter());
				int returnVal=readFC.showOpenDialog(null);
				if(returnVal==JFileChooser.APPROVE_OPTION){
					int l = getLengthFiles(files);
					files[l]=readFC.getSelectedFile();
					clear.setVisible(true);
		
					graph.remove(instruct);
			
					setHeaders();

					if (l>=4){
						open.setVisible(false);
						open.repaint();
					}			

					String headerTxt="";
					headerTxt = files[l].getPath()+"\n"+getHeader(files[l]);
					headers[l].setText(headerTxt);
					
					ylabel.setVisible(true);
					yaxis.setVisible(true);
					plot.setVisible(true);
									
			}	
			}});


		zoomTest.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				initZoomFrame(zoomVal);
			}});
		

		zoomBox.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				zoomVal=getIndexOf((String)zoomBox.getSelectedItem(),varNames);
		}});

		yaxis.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
			yAxisSelection=getIndexOf((String)yaxis.getSelectedItem(),yvars);
		}});

		plot.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				double max = 0;
				for (int k=0; k<getLengthFiles(files); k++){
					writeData(files[k],k,yvarNums[yAxisSelection]);
				}

				for (int j=0; j<9; j++){
					updateTemplate(template,j,yvarNums[yAxisSelection]);
					boolean check =callGnuplot();
					if (!check){break;}
				}

			open.setVisible(false);
			open.repaint();
			
			graph.add(plot1);
			graph.add(plot2);
			graph.add(plot3);
			graph.add(plot4);
			graph.add(plot5);

			graph.revalidate();
			graph.repaint();		
			
			swapPane(pane);	
		
			zoomBox.setVisible(true);
			zoomTest.setVisible(true);
			swap.setVisible(true);		
	
		}});

		swap.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				pane=!pane;
				if(pane){
					setZoomBox(varPane1);
				}
				else{setZoomBox(varPane2);}
				swapPane(pane);			

		}});

		quit.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				
				quitActions();
			}});
	
	clear.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){

				clearActions();

			}});
	}	

	/** This method switches which plots are visible*/
	public void swapPane(boolean c){
		
		Image img1=Toolkit.getDefaultToolkit().createImage(imageFiles[0].getName());
		Image img2=Toolkit.getDefaultToolkit().createImage(imageFiles[1].getName());
		Image img3=Toolkit.getDefaultToolkit().createImage(imageFiles[2].getName());
		Image img4=Toolkit.getDefaultToolkit().createImage(imageFiles[3].getName());
		Image img5=Toolkit.getDefaultToolkit().createImage(imageFiles[4].getName());
		Image img6=Toolkit.getDefaultToolkit().createImage(imageFiles[5].getName());
		Image img7=Toolkit.getDefaultToolkit().createImage(imageFiles[6].getName());
		Image img8=Toolkit.getDefaultToolkit().createImage(imageFiles[7].getName());
		Image img9=Toolkit.getDefaultToolkit().createImage(imageFiles[8].getName());
		if (c){
			plot1.setIcon(new ImageIcon(img1));
			plot2.setIcon(new ImageIcon(img2));
			plot3.setIcon(new ImageIcon(img3));
			plot4.setIcon(new ImageIcon(img4));
			plot5.setIcon(new ImageIcon(img5));
		}
		else{
			plot1.setIcon(new ImageIcon(img6));
			plot2.setIcon(new ImageIcon(img7));
			plot3.setIcon(new ImageIcon(img8));
			plot4.setIcon(new ImageIcon(img9));
			plot5.setIcon(null);
		}

		frame.repaint();
	}


	/** Removes all generated files and exits program */
	private void quitActions(){

                        try{
                                Runtime r = Runtime.getRuntime();
                                Process pq = r.exec("/bin/bash sct_quit.sh");
                               }

                        catch (IOException io){ System.out.println("IO Exception");}

				//exits
                                frame.setVisible(false);
                                frame.dispose();
                                System.exit(-1);
	}


	/** Sets interface back to original state without any data selected or plotted 
	and deletes generated files */
	private void clearActions(){

				graph.add(instruct);
			
                                //removes and reinitializes variables
                                for (int i=0; i<headers.length; i++){
			                 headers[i].setText("");
                                }

                                zoomBox.setVisible(false);
                                zoomTest.setVisible(false);
                                plot.setVisible(false);
                                ylabel.setVisible(false);
                                yaxis.setVisible(false);

                                wdcount=1;

                                files = new File[5];
                                rewriteFiles = new File[5];

                                graph.remove(plot1);
                                graph.remove(plot2);
                                graph.remove(plot3);
                                graph.remove(plot4);
                                graph.remove(plot5);

                                plot1=new JLabel();
                                plot2=new JLabel();
                                plot3=new JLabel();
                                plot4=new JLabel();
                                plot5=new JLabel();

                                graph.repaint();

                                clear.setVisible(false);
				swap.setVisible(false);


                        //removes files generated by plotting
                        try{
                                Process p = Runtime.getRuntime().exec("/bin/bash sct_quit.sh");
                        }
                        catch(IOException ex){System.out.println("IO Exception in clear");}

                                topLeft.repaint();
                                plot.setVisible(false);
                                open.setVisible(true);
                                open.repaint();
                                graph.revalidate();
	
}

	/** This method displays a popup window when an error occurs 
	e is the type of error
	s is the message displayed on the window
	*/
	private void displayError(String e, String s){

	errorBox=new JFrame(e);	
	//errorBox.setTitle(e);
	errorBox.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	errorBox.setSize(400,100);	
	
	JPanel box = new JPanel();
	box.setLayout(new BorderLayout());

	String mess = "<html>"+s+"</html>";
	JLabel message = new JLabel(mess);
	errorBox.add(box);

	JButton errorClear = new JButton("Clear");
	JButton errorQuit = new JButton("Quit");

	JPanel buttons = new JPanel();
	buttons.add(errorClear);
	buttons.add(errorQuit);

	errorBox.setLocation(java.awt.Toolkit.getDefaultToolkit().getScreenSize().width/2-200,java.awt.Toolkit.getDefaultToolkit().getScreenSize().height/2-50);

	box.add(message,BorderLayout.CENTER);
	box.add(buttons,BorderLayout.SOUTH);
	errorBox.setVisible(true);


	errorClear.addActionListener(new ActionListener(){
                        public void actionPerformed(ActionEvent e){
                                clearActions();
				errorBox.setVisible(false);
                        }});

        errorQuit.addActionListener(new ActionListener(){
                        public void actionPerformed(ActionEvent e){
                                quitActions();
                        }});

	}

	/** main method */
	public static void main(String[] args){

		SCT s = new SCT();
		s.initGUI();	
}

}
