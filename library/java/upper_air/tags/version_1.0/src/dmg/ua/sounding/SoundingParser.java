package dmg.ua.sounding;

import dmg.util.*;

import java.io.*;
import java.util.*;

/**
 * <p>The SoundingParser class is a class used for converting a file in some 
 * data format into a Sounding.  It is abstract so each individual type of 
 * sounding can extend it to properly parser the file.
 *
 * @author Joel Clawson
 */
public abstract class SoundingParser<T extends Sounding> {

    /**
     * A key to be used when parsing an actual release date/time.
     */
    public static final String ACTUAL_DATE = "actual";
    
    /**
     * A key to be used when parsing a nominal release date/time.
     */
    public static final String NOMINAL_DATE = "nominal";
    
    /**
     * Parse the specified file into ESCSoundings.
     * @param file The file to be parsed.
     * @return The list of soundings in the file.
     * @throws ConversionException if there is a problem converting values between data units.
     * @throws DateTimeException if there is a problem assigning a date to the sounding.
     * @throws InvalidValueException if there is a problem assigning a value to the sounding.
     * @throws InvalidValueWarning if there is a problem assigning a value to the sounding.
     * @throws IOException if there is a problem reading the data file.
     */
    public abstract List<T> parseFile(File file) 
	throws ConversionException, DateTimeException, InvalidValueException, 
	       InvalidValueWarning, IOException;
}
