package dmg.ua.sounding.extract;

import dmg.util.InvalidParameterException;

/**
 * The ESC5mbExtractor is for extracting a 5 mb pressure level frequency
 * from a sounding.  The Interpolation type specifies a 'P' for pressure.
 * This specifies that the minimum interpolation pressure is 50 mb, and 
 * the interpolation is done every 5 mb.  The Second Range notes how many
 * seconds on either side of the interpolated record can be used for the
 * interpolation.
 *
 * @author Joel Clawson
 */
public class ESC5mbExtractor extends ESCExtractor {

	@Override
	public int getInterpolationInterval() { return 5; }

	@Override
	public int getMinimumInterpolationValue() { return 50; }

	@Override
	public int getSecondRange() { return 10; }

        @Override
	public char getInterpolationType() { return 'P'; }        
        
	public static void main(String[] args) throws Exception {
		ESC5mbExtractor extractor = new ESC5mbExtractor();
		try { 
			extractor.parseArguments(args);
		}
		catch (InvalidParameterException e) {
			System.out.println(e.getMessage());
			extractor.printUsage();
			System.exit(1);
		}
		
		extractor.run();
	}
}
