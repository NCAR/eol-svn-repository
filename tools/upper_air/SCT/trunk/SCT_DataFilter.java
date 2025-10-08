/** @ Monica Jacobs- July 2011

 DataFilter.java- This java program extends the swing FileFilter in order to only allow files with the
extension of ".cls" or ".qc".

It is called from within SoundingGraph.java

*/
import java.io.File;
import javax.swing.*;

public class SCT_DataFilter extends javax.swing.filechooser.FileFilter{

	
	public boolean accept(File f){
		if (f.isDirectory()){return true;}

		String exten=getExtension(f);

		if(exten.equals("cls")){return true;}
		else if(exten.equals("qc")){return true;}

		return false;
	}



	public String getDescription(){
		return ".cls and .qc  files";
	}


	private String getExtension(File f){
		String s=f.getName();
		if(s.lastIndexOf('.')>0 && s.lastIndexOf('.')<s.length()-1){
			return s.substring(s.lastIndexOf('.')+1).toLowerCase();
		}
		return "";
	}

}
