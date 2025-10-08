package dmg.util;

import java.io.*;
import java.util.*;

/**
 * <p>The StateCodeMap is a container for holding the mappings of states to 
 * numerical codes and numerical codes to states.  Both mappings are grouped
 * by country.</p>
 * <p>The class implements the <code>Singleton</code> defined in the <i>Design
 * Patterns</i> book by the Gang of Four.</p>  
 * 
 * @author Joel Clawson
 */
public class StateCodeMap {

    // The singleton instance of the StateCodeMap.
    private static StateCodeMap codeMap;
    
    private TreeMap<String,TreeMap<Integer,String>> codeToStateMap;
    private TreeMap<String,TreeMap<String,Integer>> stateToCodeMap;
    
    /**
     * Create a new instance of a StateCodeMap.
     * @throws IOException if there is a problem reading the state code file.
     */
    private StateCodeMap() throws IOException {
	// Initialize the streams needed to read the state code file.
	InputStream inStream = getClass().getResourceAsStream("/state_codes.txt");
	BufferedReader reader = new BufferedReader(new InputStreamReader(inStream));
	
	// Create the state code maps.
	codeToStateMap = new TreeMap<String,TreeMap<Integer,String>>();
	stateToCodeMap = new TreeMap<String,TreeMap<String,Integer>>();
	
	// Loop through the lines in the data file.
	String line;
	while ((line = reader.readLine()) != null) {
	    // Set up the parsing of the data line.
	    StringTokenizer tokenizer = new StringTokenizer(line,",");
	    if (tokenizer.countTokens() != 3) {
		throw new IOException("The state code file is malformed.");
	    }
	    
	    // Parse out the different parts of the state codes
	    String country = tokenizer.nextToken().toLowerCase();
	    int code = Integer.parseInt(tokenizer.nextToken());
	    String state = tokenizer.nextToken();
	    
	    // Create the country specific maps.
	    if (!codeToStateMap.containsKey(country)) {
		codeToStateMap.put(country, new TreeMap<Integer,String>());
		stateToCodeMap.put(country, new TreeMap<String,Integer>());
	    }
	    
	    // Put the values into the maps.
	    codeToStateMap.get(country).put(code, state);
	    stateToCodeMap.get(country).put(state.toLowerCase(), code);
	}
	
	// Close the open streams.
	reader.close();
	inStream.close();
    }
    
    /**
     * Get the singleton instance of the StateCodeMap.
     * @return The StateCodeMap.
     * @throws IOException when there is a problem reading in the state code
     *  file.
     */
    public static StateCodeMap getInstance() throws IOException {
	if (codeMap == null) { codeMap = new StateCodeMap(); }
	return codeMap;
    }
    
    /**
     * Determine the state for the specified state code and country.
     * @param country The 2 character country the state is located in.
     * @param code The state code for the state.
     * @return The state the code is for.  US states will be the 2 character
     * postal abbreviation.  All other countries will be the full state name.
     * @throws ConversionException if the country is <code>null</code> or 
     * the state code/country pair cannot be found in the code map.
     */
    public String getStateCode(String country, int code) throws ConversionException {
	if (country == null) {
	    throw new ConversionException("getStateCode","country",
					  "The country was null.");
	}
	if (!codeToStateMap.containsKey(country.toLowerCase())) {
	    throw new ConversionException("getStateCode","country",
					  String.format("The country %s is not recognized in " +
							"the code map.",country));
	}
	if (!codeToStateMap.get(country.toLowerCase()).containsKey(code)) {
	    throw new ConversionException("getStateCode","code",
					  String.format("The state %d is not recoginized for " +
							"country %s.",code,country));
	}
	return codeToStateMap.get(country.toLowerCase()).get(code);
    }
    
    /**
     * Determine the state code for the specified state and country.
     * @param country The 2 character country the state is located in.
     * @param state The state the code is for.  US states must be the 2 
     * character postal abbreviations.  All other countries are the full state names.
     * @return The state code for the specified state and country.
     * @throws ConversionException if the country or state is null or the 
     * state/country pair cannot be found in the map.
     */
    public int getStateCode(String country, String state) throws ConversionException {
	if (country == null) {
	    throw new ConversionException("getStateCode","country",
					  "The country was null.");
	}
	if (state == null) {
	    throw new ConversionException("getStateCode","state",
					  "The state was null.");
	}
	if (!stateToCodeMap.containsKey(country.toLowerCase())) {
	    throw new ConversionException("getStateCode","country",
					  String.format("The country %s is not recognized in" +
							" the state map.",country));
	}
	if (!stateToCodeMap.get(country.toLowerCase()).
	    containsKey(state.toLowerCase())) {
	    throw new ConversionException("getStateCode","state",
					  String.format("The state %s is not recognized for" +
							" country %s.",state,country));
	}
	return stateToCodeMap.get(country.toLowerCase()).
	    get(state.toLowerCase());
    }
}
