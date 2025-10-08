package milagro.question.parser;

import java.io.*;
import java.util.*;

public class QuestionnaireParser {
    
    private static final String G1_REPORT = "/source_files/MILAGRO_G1.csv";
    private static final String INSTRUMENT_OUTPUT_DIR = "output/instrument";
    private static final String LIST_OUTPUT_DIR = "output/lists";
    private static final String REPORT = "/source_files/MILAGRODataArchive.txt";

    private int completelyEmpty,incompleteRecords;
    private Set<String> archiveLocations, emails, institutions, instruments, parameters, pis, siteLocations;
    
    public QuestionnaireParser() {
	archiveLocations = new TreeSet<String>();
	emails = new TreeSet<String>();
	institutions = new TreeSet<String>();
	instruments = new TreeSet<String>();
	parameters = new TreeSet<String>();
	pis = new TreeSet<String>();
	siteLocations = new TreeSet<String>();
    }
    
    private void cleanDirectory(String name) {
	File directory = new File(name);
	if (directory.exists()) {
	    File[] fileList = directory.listFiles();
	    for (File file: fileList) {
		file.delete();
	    }
	}
    }
    
    private String determineNextG1Token(StringTokenizer lineTokenizer, StringTokenizer copyTokenizer) throws IOException {
	String lineToken = lineTokenizer.nextToken();
	String copyToken = copyTokenizer.nextToken();
	
	if (lineToken.equals(copyToken)) { return String.format("\"%s\"", lineToken); }
	
	if (copyToken.equals(" ")) {
	    StringBuffer sb = new StringBuffer(lineToken);
	    while (!sb.toString().endsWith("\"")) {
		sb.append(",");
		lineToken = lineTokenizer.nextToken();
		sb.append(lineToken);
	    }
	    return sb.toString();
	} else {
	    throw new IOException("Token should have been the empty String.  "+copyToken);
	}
    }
    
    private void generateListFile(File directory, String filename, Set<String> listData) throws IOException {
	PrintWriter out = new PrintWriter(new FileWriter(new File(directory, filename)));
	for (String entry: listData) {
	    out.println(entry);
	}
	out.close();
    }
    
    private List<QuestionnaireEntry> parseDataLine(String line, int lineNumber) throws IOException {
	List<QuestionnaireEntry> entryList = new ArrayList<QuestionnaireEntry>();

        if (line.endsWith(";;en;;;;########################################;;########################################")) {
	    completelyEmpty++;
            return entryList;
        }
	
	while (line.contains("##")) {
	    line = line.replace("##", "#\"\"#");
	}
        while (line.contains(";;")) {
            line = line.replace(";;", ";\"\";");
        }
	
	StringTokenizer tokenizer = new StringTokenizer(line, ";");
	if (tokenizer.countTokens() != 10) {
	    throw new IOException(String.format("The tokens in line number %d is not 10.  It was %d.", lineNumber, tokenizer.countTokens()));
	}
	
	int entryId = Integer.parseInt(tokenizer.nextToken());
	tokenizer.nextToken(); // Parse off the entry start date
	String endDate = tokenizer.nextToken(); // Parse off the entry end date
	if (endDate.trim().equals("\"\"")) {
	    incompleteRecords++;
	}
	tokenizer.nextToken(); // Parse off the entry language
	String pi = tokenizer.nextToken();
	String institution = tokenizer.nextToken();
	String email = tokenizer.nextToken();

	entryList.addAll(parseInstrumentation(entryId, 1, pi, institution, email, tokenizer.nextToken()));
	if (Integer.parseInt(tokenizer.nextToken()) == 1) {
	    entryList.addAll(parseInstrumentation(entryId, 2, pi, institution, email, tokenizer.nextToken()));
	}
	
	return entryList;
    }
    
    private Date parseDate(String date) {
	if (date.equals("\"\"")) { return null; }
	
	Calendar calendar = Calendar.getInstance();
	calendar.set(Integer.parseInt(date.substring(7, 9)) + 2000,
		     Integer.parseInt(date.substring(4, 6)) - 1,
		     Integer.parseInt(date.substring(1, 3)), 0, 0, 0);
	return calendar.getTime();
    }
    
    private QuestionnaireEntry parseG1DataLine(String line, int lineNumber) throws IOException {
	// Create a copy of the line without any quoted data.
	String copy = line.replaceAll("\".*\"", " ");
	
	StringTokenizer lineTokenizer = new StringTokenizer(line, ",");
	StringTokenizer copyTokenizer = new StringTokenizer(copy, ",");
	
	QuestionnaireEntry entry = new QuestionnaireEntry("G1", lineNumber, 1);
	entry.setPI(determineNextG1Token(lineTokenizer, copyTokenizer));
	entry.setInstitution(determineNextG1Token(lineTokenizer, copyTokenizer));
	entry.setEmail(determineNextG1Token(lineTokenizer, copyTokenizer));
	
	entry.setInstrument(determineNextG1Token(lineTokenizer, copyTokenizer));
	entry.setSite(determineNextG1Token(lineTokenizer, copyTokenizer), null);
	entry.setTime(parseG1Date(determineNextG1Token(lineTokenizer, copyTokenizer)), 
		      parseG1Date(determineNextG1Token(lineTokenizer, copyTokenizer)));
	entry.setParameters(determineNextG1Token(lineTokenizer, copyTokenizer));
	entry.setArchive(determineNextG1Token(lineTokenizer, copyTokenizer), 
			 determineNextG1Token(lineTokenizer, copyTokenizer));
	
	return entry;
    }
    
    private Date parseG1Date(String date) {
	if (date.equals("\"\"")) { return null; }
	
	Calendar calendar = Calendar.getInstance();
	calendar.set(Integer.parseInt(date.substring(7, 9)) + 2000,
		     Integer.parseInt(date.substring(1, 3)) - 1,
		     Integer.parseInt(date.substring(4, 6)), 0, 0, 0);
	return calendar.getTime();
	
    }
    
    private List<QuestionnaireEntry> parseInstrumentation(int entryId, int matrix, String pi, String institution, String email, String data) {
	List<QuestionnaireEntry> entryList = new ArrayList<QuestionnaireEntry>();
	data = data+"#\"\"#";
	
	StringTokenizer tokenizer = new StringTokenizer(data, "#");
	
	for (int offset = 0; offset < 5; offset++) {
	    String instrumentName = tokenizer.nextToken();
	    String siteName = tokenizer.nextToken();
	    String otherSite = tokenizer.nextToken();
	    Date startDate = parseDate(tokenizer.nextToken());
	    Date endDate = parseDate(tokenizer.nextToken());
	    String params = tokenizer.nextToken();
	    String archive = tokenizer.nextToken();
	    String otherArchive = tokenizer.nextToken();
	    
	    
	    QuestionnaireEntry entry = new QuestionnaireEntry("Questionnaire", entryId, (matrix - 1) * 5 + offset + 1);
	    entry.setPI(pi);
	    entry.setInstitution(institution);
	    entry.setEmail(email);
	    
	    entry.setInstrument(instrumentName);
	    entry.setSite(siteName, otherSite);
	    entry.setTime(startDate, endDate);
	    entry.setParameters(params);
	    entry.setArchive(archive, otherArchive);
	    
	    if (entry.getInstrument() != null || entry.getSite() != null || entry.getAlternateSite() != null ||
		entry.getStartDate() != null || entry.getEndDate() != null || entry.getParameters() != null ||
		entry.getArchive() != null || entry.getAlternateArchive() != null) {
		entryList.add(entry);
	    }
	}
	
	return entryList;
    }
    
    private List<QuestionnaireEntry> parseRawG1Report() throws IOException {
	List<QuestionnaireEntry> list = new ArrayList<QuestionnaireEntry>();
	
	BufferedReader reader = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream(G1_REPORT)));
	
	// Parse off the header line.
	reader.readLine();
	
	int lineCount = 1;
	String line;
	while ((line = reader.readLine()) != null) {
	    list.add(parseG1DataLine(line, ++lineCount));
	}
	
	reader.close();
	
	return list;
    }
    
    private List<QuestionnaireEntry> parseRawReport() throws IOException {
	List<QuestionnaireEntry> list = new ArrayList<QuestionnaireEntry>();
	
	BufferedReader reader = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream(REPORT)));
	
	// Parse off the header line.
	reader.readLine();
	
	int lineCount = 1;
	String line;
	while ((line = reader.readLine()) != null) {
	    list.addAll(parseDataLine(line, ++lineCount));
	}
	
	reader.close();
	
	return list;
    }
    
    public void parseRawReportFiles() throws IOException {
	List<QuestionnaireEntry> list = parseRawReport();
	list.addAll(parseRawG1Report());
	
	cleanDirectory(INSTRUMENT_OUTPUT_DIR);
	
	for (QuestionnaireEntry entry: list) {
	    // Determine the filename to use for this entry
	    String filename = entry.getPI();
	    filename = filename.replaceAll("\\s+", "_");
	    filename = filename.replaceAll("[:\\.\\/,]", "");			
	    filename = filename.replaceAll("_+", "_");
	    File file = new File(INSTRUMENT_OUTPUT_DIR, String.format("%s_01.txt", filename));
	    int index = 1;
	    while (file.exists()) {
		file = new File(INSTRUMENT_OUTPUT_DIR, String.format("%s_%02d.txt", filename, index++));
	    }
	    
	    
	    PrintWriter out = new PrintWriter(new FileWriter(file));
	    out.println(entry);
	    out.close();
	    
	    if (entry.getParameters() != null) {
		StringTokenizer paramTokenizer = new StringTokenizer(entry.getParameters(), ",");
		while (paramTokenizer.hasMoreTokens()) {
		    parameters.add(paramTokenizer.nextToken().trim());
		}
	    }
	    
	    if (entry.getArchive() != null) { archiveLocations.add(entry.getArchive()); }
	    if (entry.getAlternateArchive() != null) { archiveLocations.add(entry.getAlternateArchive()); }
	    if (entry.getEmail() != null) { emails.add(entry.getEmail()); }
	    if (entry.getInstitution() != null) { institutions.add(entry.getInstitution()); }
	    if (entry.getInstrument() != null) { instruments.add(entry.getInstrument()); }
	    if (entry.getPI() != null) { pis.add(entry.getPI()); }
	    if (entry.getSite() != null) { siteLocations.add(entry.getSite()); }
	    if (entry.getAlternateSite() != null) { siteLocations.add(entry.getAlternateSite()); }
	}
	
	
	cleanDirectory(LIST_OUTPUT_DIR);
	generateListFile(new File(LIST_OUTPUT_DIR), "instruments.txt", instruments);
	generateListFile(new File(LIST_OUTPUT_DIR), "parameters.txt", parameters);
	generateListFile(new File(LIST_OUTPUT_DIR), "archive_locations.txt", archiveLocations);
	generateListFile(new File(LIST_OUTPUT_DIR), "site_locations.txt", siteLocations);
	generateListFile(new File(LIST_OUTPUT_DIR), "pis.txt", pis);
	generateListFile(new File(LIST_OUTPUT_DIR), "emails.txt", emails);
	generateListFile(new File(LIST_OUTPUT_DIR), "institutions.txt", institutions);

	System.out.printf("There were %d completely missing records.\n", completelyEmpty);
	System.out.printf("There were %d incomplete records.\n", incompleteRecords);
    }
}
