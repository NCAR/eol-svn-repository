package dmg.ua.sounding.convert.mips;

import java.io.*;
import java.util.*;
import java.util.zip.*;

import dmg.ua.sounding.esc.*;
import dmg.util.*;

public class MipsConverter implements FilenameFilter {

    private boolean gzip;
    private String finalDirectory, outputDirectory, project, rawDirectory;

    public MipsConverter() {
	gzip = true;
    }

    public boolean accept(File directory, String name) {
	return name.startsWith("D") && (name.endsWith(".cls") || name.endsWith(".cls.gz"));
    }

    public void execute() {
	System.out.println("**************  WARNING ***************");
	System.out.println(" This does not generate a station list ");
	System.out.println("**************  WARNING ***************");


	for (File file: findRawFiles()) {
	    System.out.printf("Processing file:  %s ...\n", file.getName());

	    ESCSounding sounding = null;
	    try { sounding = parse(file); }
	    catch (CalculationWarning e) {
		System.err.printf("Unable to parse file %s.  %s\n", file.getName(), e.getMessage());
	    }
	    catch (ConversionException e) {
		System.err.printf("Unable to parse file %s.  %s\n", file.getName(), e.getMessage());
	    }
	    catch (DateTimeException e) {
		System.err.printf("Unable to parse file %s.  %s\n", file.getName(), e.getMessage());
	    }
	    catch (InvalidValueException e) {
		System.err.printf("Unable to parse file %s.  %s\n", file.getName(), e.getMessage());
	    }
	    catch (InvalidValueWarning e) {
		System.err.printf("Unable to parse file %s.  %s\n", file.getName(), e.getMessage());
	    }
	    catch (IOException e) {
		System.err.printf("Unable to parse file %s.  %s\n", file.getName(), e.getMessage());
	    }

	    if (sounding != null) {
		try { write(sounding); }
		catch (IOException e) {
		    System.err.printf("Unable to write file %s.  %s\n", file.getName(), e.getMessage());
		}
	    }
	}
    }

    public List<File> findRawFiles() {
	return new ArrayList<File>(Arrays.asList((new File(getRawDirectory())).listFiles(this)));
    }

    public String getProject() { return project; }

    public String getOutputDirectory() { return outputDirectory; }

    public String getRawDirectory() { return rawDirectory; }

    public boolean gzipOutputFiles() { return gzip; }

    public BufferedReader openReadFile(File file) throws IOException {
	if (file.getName().toLowerCase().endsWith(".gz")) {
	    return new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(file))));
	} else {
	    return new BufferedReader(new FileReader(file));
	}
    }

    public PrintWriter openWriteFile(File file) throws IOException {
	if (file.getName().toLowerCase().endsWith(".gz")) {
	    return new PrintWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(file))));
	} else {
	    return new PrintWriter(new FileWriter(file));
	}
    }

    public ESCSounding parse(File file) throws CalculationWarning, ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning, IOException {
	BufferedReader reader = openReadFile(file);

	ESCSounding sounding = new ESCSounding();
	sounding.setProjectId(getProject());
	sounding.setHeaderLine(9, "Input File:", file.getName());

	for (int i = 0; i < 15; i++) {
	    String line = reader.readLine();
	    if (i < 12) { parseHeaderLine(sounding, line); }
	}

	String line = null;
	while ((line = reader.readLine()) != null) {
	    sounding.add(parseRecordLine(line));
	}

	reader.close();

	return sounding;
    }

    public void parseArguments(String[] args) {
	for (String arg: args) {
	    String[] pieces = arg.split("\\s");

	    if      (pieces[0].equals("-f")) { setFinalDirectory(pieces[1]); }
            else if (pieces[0].equals("-o")) { setOutputDirectory(pieces[1]); }
            else if (pieces[0].equals("-P")) { setProject(pieces[1]); }
	    else if (pieces[0].equals("-r")) { setRawDirectory(pieces[1]); }
	    else if (pieces[0].equals("-Z")) { setGzipOutputFiles(false); }
	}
    }

    public void parseHeaderLine(ESCSounding sounding, String line) throws ConversionException, DateTimeException, InvalidValueException, InvalidValueWarning {
	if (line.equals("/")) { return; }
	else if (line.startsWith("Data Type:")) { sounding.setDataType("High Resolution Sounding"); }
	else if (line.startsWith("Project ID:")) {
	    sounding.setStationId("bx3");
	    sounding.setStationDescription(line.substring(35).trim());
	}
	else if (line.startsWith("Launch Site Type/Site ID:")) {
	    sounding.setHeaderLine(6, "Sonde Type/ID/Sensor ID/Tx Freq:", line.substring(35).trim());
	}
	else if (line.startsWith("Launch Location (lon,lat,alt):")) {
	    List<String> pieces = new ArrayList<String>(Arrays.asList(line.substring(35).trim().split("\\s+")));
	    Collections.reverse(pieces);
	    sounding.setLongitude(Double.parseDouble(pieces.get(2)));
	    sounding.setLatitude(Double.parseDouble(pieces.get(1)));
	    sounding.setAltitude(Double.parseDouble(pieces.get(0)), LengthUtils.METERS);
	}
	else if (line.startsWith("GMT Launch Time (y,m,d,h,m,s):")) {
	    String[] datePieces = line.substring(35).trim().split("(,\\s)|(:)");
	    sounding.setActualRelease(Integer.parseInt(datePieces[0]),
				      Integer.parseInt(datePieces[1]),
				      Integer.parseInt(datePieces[2]),
				      Integer.parseInt(datePieces[3]),
				      Integer.parseInt(datePieces[4]),
				      Integer.parseInt(datePieces[5]), TimeUtils.UTC);
	    sounding.setNominalRelease(sounding.getActualDate());
	}
	else if (line.startsWith("Sonde Id:")) {
	    sounding.setHeaderLine(8, "Radiosonde Serial Number:", line.substring(35).trim());
	}
	else if (line.startsWith("System Operator/Comments:")) {
	    sounding.setHeaderLine(7, "System Operator/Comments:", line.substring(35).trim());
	}
    }

    public ESCSoundingRecord parseRecordLine(String line) throws CalculationWarning, ConversionException, InvalidValueException, InvalidValueWarning {
	String[] data = line.trim().split("\\s+");

	ESCSoundingRecord record = new ESCSoundingRecord(false);
	
	if (!data[0].equals("9999.0")) { record.setTime(Double.parseDouble(data[0])); }
	if (!data[1].equals("9999.0")) { record.setPressure(Double.parseDouble(data[1]), PressureUtils.MILLIBARS); }
	if (!data[2].equals("999.0")) { record.setTemperature(Double.parseDouble(data[2]), TemperatureUtils.CELCIUS); }
	if (!data[3].equals("999.0")) { record.setDewPoint(Double.parseDouble(data[3]), TemperatureUtils.CELCIUS); }
	if (!data[4].equals("999.0")) { record.setRelativeHumidity(Double.parseDouble(data[4])); }
	if (!data[5].equals("999.0")) { record.setUComponent(Double.parseDouble(data[5]), VelocityUtils.METERS_PER_SECOND); }
	if (!data[6].equals("999.0")) { record.setVComponent(Double.parseDouble(data[6]), VelocityUtils.METERS_PER_SECOND); }
	if (!data[7].equals("999.0")) { record.setWindSpeed(Double.parseDouble(data[7]), VelocityUtils.METERS_PER_SECOND); }
	if (!data[8].equals("999.0")) { record.setWindDirection(Double.parseDouble(data[8])); }
	if (!data[9].equals("99.0")) { record.setAscentRate(Double.parseDouble(data[9]), VelocityUtils.METERS_PER_SECOND); }
	if (!data[10].equals("999.000")) { record.setLongitude(Double.parseDouble(data[10])); }
	if (!data[11].equals("999.000")) { record.setLatitude(Double.parseDouble(data[11])); }
	if (!data[14].equals("99999.0")) { record.setAltitude(Double.parseDouble(data[14]), LengthUtils.METERS); }

	return record;
    }

    public void setFinalDirectory(String finalDirectory) { this.finalDirectory = finalDirectory; }

    public void setGzipOutputFiles(boolean flag) { gzip = flag; }

    public void setOutputDirectory(String outputDirectory) { this.outputDirectory = outputDirectory; }

    public void setProject(String project) { this.project = project; }

    public void setRawDirectory(String rawDirectory) { this.rawDirectory = rawDirectory; }

    public void write(ESCSounding sounding) throws IOException {
	String filename = String.format("%1$s_%2$tY%2$tm%2$td%2$tH%2$tM%2$tS.cls%3$s",
					sounding.getStationId(), sounding.getActualDate(),
					gzipOutputFiles() ? ".gz" : "");

	File outputDir = new File(getOutputDirectory());
	outputDir.mkdirs();

	PrintWriter out = openWriteFile(new File(outputDir, filename));
	out.println(sounding.toString());
	out.close();
    }

    public static void main(String[] args) {
	MipsConverter converter = new MipsConverter();
	
	converter.parseArguments(args);
	converter.execute();
    }
}