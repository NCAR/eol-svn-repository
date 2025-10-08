package dmg.ua.sounding.extract;

import dmg.util.InvalidParameterException;

/**
 * The ESC50mExtractor is for extracting a 50 m height level frequency
 * from a sounding.  The Interpolation type specifies an 'H' for height.
 * This specifies that the minimum interpolation height is 50 m, and 
 * the interpolation is done every 50 m.  The Second Range notes how many
 * seconds on either side of the interpolated record can be used for the
 * interpolation. 
 *
 * Note that this routine increases 50 m for each interpolated record,
 * whereas the 5 mb routine decreases 5 mb for each interpolated record.
 *
 * @author J. Scannell
 */

public class ESC50mExtractor extends ESCExtractor {

	@Override
	public int getInterpolationInterval() { return 50; }

	@Override
	public int getMinimumInterpolationValue() { return 50; }

	@Override
	public int getSecondRange() { return 10; }

        @Override
	public char getInterpolationType() { return 'H'; }
        
	public static void main(String[] args) throws Exception {
		ESC50mExtractor extractor = new ESC50mExtractor();
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
