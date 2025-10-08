package meta;

public class UploadUtils {

	public UploadUtils() { }

	public static String fixFileBadCharacters(String inString) {
		System.out.println("****** fileHasBadCharacters: starting input=" + inString + "************");
		try {
			StringBuilder in = new StringBuilder(inString);
			int size= in.length();
			int a = "a".codePointAt(0);
			int A = "A".codePointAt(0);
			int z = "z".codePointAt(0);
			int Z = "Z".codePointAt(0);
			int zero = "0".codePointAt(0);
			int colon = ":".codePointAt(0);
			int dot = ".".codePointAt(0);
			int minus = "-".codePointAt(0);
			int underscore = "_".codePointAt(0);
			int space = " ".codePointAt(0);
			
			for(int i= 0; i < size; i++){
				int utfChar = in.codePointAt(i);
				
				if( (utfChar >= a) && (utfChar <= z)) continue;
				if( (utfChar >= A) && (utfChar <= Z)) continue;
				if( (utfChar >= zero) && (utfChar <= colon)) continue;
				if((i + 1 < size - 1) && (utfChar == dot)) {
					if( !(in.codePointAt(i+1) == dot)) continue;
				}
				if ((i+1 == size - 1) && (utfChar == dot)) continue;
				if(  utfChar == minus) continue;
				if(  utfChar == underscore) continue;
				//if(  utfChar == space) continue;
				System.out.println("bad char found at " + i + " char=" + utfChar);
				in.setCharAt(i,'_');
			}
			return in.toString();
		}
		catch(Exception e) {
			return null;
		}
	}

	public static String removeWindowsExtraStuff(String inputString) {
	  System.out.println("UploadMultipleFiles: removeWindowsExtraStuff = " + inputString );

		if(inputString.indexOf(":") == 1){
		 int li =  inputString.lastIndexOf("\\");
		 if (li >=0 ) {
			inputString  = inputString.substring(li+ 1);
			System.out.println("new input string="+ inputString);
		 }
		}
	  return inputString;
	}
	
}
