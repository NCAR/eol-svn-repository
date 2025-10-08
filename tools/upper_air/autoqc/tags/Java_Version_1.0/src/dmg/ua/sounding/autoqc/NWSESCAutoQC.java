package dmg.ua.sounding.autoqc;

import static dmg.ua.sounding.esc.ESCSoundingRecord.BAD_FLAG;
import static dmg.ua.sounding.esc.ESCSoundingRecord.QUESTIONABLE_FLAG;
import dmg.ua.sounding.esc.*;
import dmg.ua.sounding.esc.ESCSoundingRecord.ESCFlag;
import dmg.util.InvalidParameterException;

import java.io.PrintWriter;
import java.util.*;

/**
* <p>The NWSESCAutoQC class is an extension of the UpsondeAutoQC for running
* the automatic quality control (autoQC) for MicroArt NWS radiosonde data.  This 
* extension provides some special checks for the NWS soundings.</p>
* 
* @author Joel Clawson
* @version 1.0
* <p>Version 1.0 is the upsonde extraction of the autoQC when it was ported
* from C/C++ into Java in the summer of 2007.  It contains only the NWS
* radiosonde specific functions.  All of the general functionality is in the 
* base UpsondeESCAutoQC and  ESCAutoQC classes including the version
* information.</p>
*/
public class NWSESCAutoQC extends UpsondeESCAutoQC {

	/**
	 * Create a new instance of a NWSESCAutoQC.
	 * @param propertyFile The file containing the QC limits.
	 * @throws QCException when there is a problem reading in the limits from
	 * the properties file.
	 */
	public NWSESCAutoQC(String propertyFile) throws QCException { 
		super(propertyFile);
	}
	
	/**
	 * Check the change in ascent rate for the end points in the specified list
	 * of soundings.
	 * @param records The list of records to use in the check.
	 * @param previousAscentRate The last calculated ascent rate.
	 * @param begin A flag that defines if this is the first time the ascent
	 * rate check has been called.
	 * @param log The stream where errors are to be written.
	 * @return The calculated ascent rate between the end points of the records.
	 */
	public Double ascentRateChange(List<ESCSoundingRecord> records,
			Double previousAscentRate, boolean begin, PrintWriter log) {
		
		// Initialize the variables used in the function.
		ESCSoundingRecord current = records.get(0);
		ESCSoundingRecord next = records.get(records.size() - 1);
		double ascentRate = 0.0;
		
		// Calculate the ascent rate between the end points.
		if (current.getTime() != next.getTime() &&
				current.getAltitude() != next.getAltitude()) {
			ascentRate = ((next.getAltitude() - current.getAltitude()) /
					(next.getTime() - current.getTime()));
		}
		
		// Do the check if the ascent rate has been checked before.
		if (!begin) {
			double rate = ascentRate - previousAscentRate;
			// Determine if the ascent rate change is at least questionable.
			if (Math.abs(rate) >= limits.getQuestionableAscentRateChange()) {
				
				log.printf("Rapid ascendrate change between pressure %6.1f " +
						"and pressure %6.1f is %6.1f m/s.\n", 
						current.getPressure(), next.getPressure(), rate);

				// Determine if the change was BAD or just QUESTIONABLE
				ESCFlag flag =
					Math.abs(rate) >= limits.getBadAscentRateChange() ?
							BAD_FLAG : QUESTIONABLE_FLAG;
					
				// Update the pressure flags for all of the records between
				// the two end points.
				for (ESCSoundingRecord record: records) {
					updatePressureFlag(record, flag);
				}
			}
		}

		// Return the calculated ascent rate.
		return ascentRate;
	}
	
	@Override
	public boolean canCheckPressureRates(ESCSoundingRecord record) {
		return record.getPressure() > 100.0;
	}
		
	@Override
	public void qcNWS(List<ESCSoundingRecord> records, PrintWriter log) {
		Double previousRate = 0.0;
		boolean beginning = true;
		
		for (int endIndex = 5; endIndex < records.size(); endIndex++) {			

			// Define the end points to use in the checks.
			ESCSoundingRecord start = records.get(endIndex - 5);
			ESCSoundingRecord end = records.get(endIndex);
			
			if (start.getPressure() != null && end.getPressure() != null &&
					start.getPressure() <= 100.0) {
				
				if (start.getAltitude() != null && end.getAltitude() != null &&
						start.getTime() != null && end.getTime() != null) {
					previousRate = ascentRateChange(
							records.subList(endIndex - 5, endIndex + 1), 
							previousRate, beginning, log);

					if (beginning) { beginning = false; }
				}
				
				rapidPressureCheck(records, log);
			}
		}
	}

	/**
	 * Run the auto QC program for NWS radiosonde data.
	 * @param args The list of command line arguments provided by the user.
	 * @throws QCException if there is a problem creating a new NWS upsonde
	 * auto QC instance.
	 */
	public static void main(String[] args) throws Exception {
		NWSESCAutoQC qc = new NWSESCAutoQC(args[0]);
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
