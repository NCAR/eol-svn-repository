package dmg.ua.sounding.extract;

import dmg.util.InvalidParameterException;

public class ESC5mbExtractor extends ESCExtractor {

	@Override
	public int getInterpolationInterval() { return 5; }

	@Override
	public int getMinimumInterpolationPressure() { return 50; }

	@Override
	public int getSecondRange() { return 10; }

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
