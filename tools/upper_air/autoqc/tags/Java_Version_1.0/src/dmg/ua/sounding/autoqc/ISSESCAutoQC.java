package dmg.ua.sounding.autoqc;

import dmg.ua.sounding.esc.ESCSoundingRecord;
import dmg.util.InvalidParameterException;

/**
 * <p>The ISSESCAutoQC class is an extension of the UpsondeESCAutoQC for running
 * the automatic quality control (autoQC) for EOL/ISS radiosonde data.  The ISS
 * extension overrides some of the general upsonde functions for the ISS QC.</p>
 * 
 * @author Joel Clawson
 * @version 1.0
 * <p>Version 1.0 is the ISS extraction of the autoQC when it was ported
 * from C/C++ into Java in the summer of 2007.  It contains only the ISS
 * specific functions.  All of the general functionality is in the base
 * UpsondeESCAutoQC and ESCAutoQC classes including the version information.</p>
 */
public class ISSESCAutoQC extends UpsondeESCAutoQC {

	/**
	 * Create a new instance of an ISSESCAutoQC.
	 * @param propertyFile The file that contains the QC limits.
	 * @throws QCException if there is a problem reading in the QC limits
	 * from the properties file.
	 */
	public ISSESCAutoQC(String propertyFile) throws QCException { 
		super(propertyFile);
	}
	
	@Override
	public boolean isDecreasingPressure(ESCSoundingRecord current, 
			ESCSoundingRecord next) {
		return current.getPressure() >= next.getPressure();
	}
	
	/**
	 * Run the auto QC program for EOL/ISS radiosonde data.
	 * @param args The list of command line arguments provided by the user.
	 * @throws QCException if there is a problem creating a new ISS upsonde
	 * auto QC instance.
	 */
	public static void main(String[] args) throws Exception {
		ISSESCAutoQC qc = new ISSESCAutoQC(args[0]);
		try { 
			qc.parseArguments(args);
		}
		catch (InvalidParameterException e) {
			System.out.println(e.getMessage());
			qc.printUsage();
			System.exit(1);
		}
		
		qc.run();
	}
}
