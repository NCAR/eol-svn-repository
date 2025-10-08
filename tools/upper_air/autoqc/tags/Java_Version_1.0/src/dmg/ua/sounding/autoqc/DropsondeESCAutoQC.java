package dmg.ua.sounding.autoqc;

import java.io.PrintWriter;
import java.util.*;
import dmg.ua.sounding.esc.ESCSoundingRecord;
import dmg.util.InvalidParameterException;

/**
 * <p>The DropsondeESCAutoQC class is an extension of the ESCAutoQC for running
 * the automatic quality control (autoQC) for dropsonde data.  The dropsonde
 * extension is required to handle the decreasing times of the records in a 
 * sounding since the records should be ordered by pressure (with the surface
 * record being the first record after the header).</p>
 * 
 * 
 * @author Joel Clawson
 * @version 1.0
 * <p>Version 1.0 is the dropsonde extraction of the autoQC when it was ported
 * from C/C++ into Java in the summer of 2007.  It contains only the dropsonde
 * specific functions.  All of the general functionality is in the base
 * ESCAutoQC class including the version information.</p>
 * <p>The time check function has some slightly modified functionality from the
 * original C/C++ version.  The lastTime variable now returns the lastTime from
 * the next record.  In the C/C++ version, it only returned that value if there
 * was a problem or was the only value available.</p>
 */
public class DropsondeESCAutoQC extends ESCAutoQC {

	/**
	 * Create a new instance of a DropsondeESCAutoQC.
	 * @param propertyFile The name of the file containing the QC limits.
	 * @throws QCException if there is a problem loading the QC limits from the
	 * properties file.
	 */
	public DropsondeESCAutoQC(String propertyFile) throws QCException { 
		super(propertyFile);
	}
	
	@Override
	public boolean canCheckPressureRates(ESCSoundingRecord record) {
		return true;
	}

	@Override
	public boolean isDecreasingPressure(ESCSoundingRecord current,
			ESCSoundingRecord next) {
		return current.getPressure() > next.getPressure();
	}

	@Override
	public void qcNWS(List<ESCSoundingRecord> records, PrintWriter log) {}
	
	@Override
	public Double timeCheck(ESCSoundingRecord current, ESCSoundingRecord next,
			Double previousTime, PrintWriter log) {
		Double lastTime = previousTime;
		
		// Handle the case where both records have valid times.
		if (current.getTime() != null && next.getTime() != null) {
			lastTime = next.getTime();
			if (current.getTime().compareTo(next.getTime()) <= 0) {
				log.printf("Time increase or equal time: %7.1f and %7.1f\n",
						current.getTime(), next.getTime());
				log.flush();
			}
		}
		
		// Handle the case where the first record doesn't have a valid time.
		else if (previousTime != null && next.getTime() != null) {
			lastTime = next.getTime();
			if (previousTime.compareTo(next.getTime()) <= 0) {
				log.printf("Time increase or equal time: %7.1f and %7.1f\n",
						previousTime, next.getTime());
				log.flush();
			}
		}
		
		// Can't compare anything, so just use the next time if it exists.
		else if (next.getTime() != null) {
			lastTime = next.getTime();
		}
		
		// If none of the conditions were met, return the previous time that
		// was passed in.
		return lastTime;
	}

	/**
	 * Run the auto QC program for dropsonde data.
	 * @param args The list of command line arguments provided by the user.
	 * @throws QCException if there is a problem creating a new dropsonde
	 * auto QC instance.
	 */
	public static void main(String[] args) throws QCException {
		DropsondeESCAutoQC qc = new DropsondeESCAutoQC(args[0]);
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
