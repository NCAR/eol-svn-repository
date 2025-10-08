package dmg.ua.sounding.dayfile;

import java.io.*;
import java.util.*;
import java.util.zip.*;

import org.junit.*;

public abstract class DayFileCreatorTest {
	
	protected static final File dataDir = new File("data");
	protected static final File outputDir = new File("dayfiles");
	
	protected static final File sounding1cls = 
		new File("data/OAK_200603011100.cls");
	protected static final File sounding1gz = 
		new File( "data/OAK_200603011100.cls.gz");
	protected static final File sounding2cls = 
		new File( "data/subdir/OAK_200603012301.cls");
	protected static final File sounding2gz = 
		new File( "data/subdir/OAK_200603012301.cls.gz");
	protected static final File sounding3cls = 
		new File( "data/subdir/NKX_200603011125.cls");
	protected static final File sounding3gz = 
		new File( "data/subdir/NKX_200603011125.cls.gz");
	protected static final File sounding4cls = 
		new File( "data/NKX_200603012320.cls");
	protected static final File sounding4gz =
		new File( "data/NKX_200603012320.cls.gz");
	

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		dataDir.mkdir();
		File subDir = new File(dataDir, "subdir");
		subDir.mkdir();
		createSounding(sounding1cls, sounding1);
		createSounding(sounding1gz, sounding1);
		createSounding(sounding2cls, sounding2);
		createSounding(sounding2gz, sounding2);
		createSounding(sounding3cls, sounding3);
		createSounding(sounding3gz, sounding3);
		createSounding(sounding4cls, sounding4);
		createSounding(sounding4gz, sounding4);
	}

	private static void createSounding(File file, String data) 
	throws IOException {
		PrintWriter out = null;
		if (file.getName().endsWith(".gz")) {
			out = new PrintWriter(new OutputStreamWriter(
					new GZIPOutputStream(new FileOutputStream(file))));
		} else {
			out = new PrintWriter(new FileWriter(file));
		}
		out.println(data);
		out.close();
	}
	
	@After
	public void tearDown() throws Exception {
		List<File> files = new ArrayList<File>();
		files.add(outputDir);
		while (!files.isEmpty()) {
			if (files.get(0).isDirectory() && 
					files.get(0).listFiles().length > 0) {
				files.addAll(0, Arrays.asList(files.get(0).listFiles()));
			} else {
				files.remove(0).delete();
			}
		}		
	}
	
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		List<File> files = new ArrayList<File>();
		files.add(dataDir);
		files.add(outputDir);
		while (!files.isEmpty()) {
			if (files.get(0).isDirectory() && 
					files.get(0).listFiles().length > 0) {
				files.addAll(0, Arrays.asList(files.get(0).listFiles()));
			} else {
				files.remove(0).delete();
			}
		}
	}
	
	protected static final String sounding1 = "Data Type:                         National Weather Service Sounding/Unspecified\nProject ID:                        0\nRelease Site Type/Site ID:         OAK Oakland, CA\nRelease Location (lon,lat,alt):    122 12.00'W, 37 42.00'N, -122.2,  37.7,    2.0\nUTC Release Time (y,m,d,h,m,s):    2006, 03, 01, 11:00:00\nAscension No:                      1153\nRadiosonde Serial Number:          84966633.CSN\nRadiosonde Manufacturer:           VIZ B2\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):2006, 03, 01, 12:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n   0.0 1021.2   7.7   6.2  90.0   -1.0    0.4   1.1 111.8 999.0 -122.200  37.700 999.0 999.0     2.0  2.0  2.0  2.0  1.0  1.0  9.0\n   6.0 1011.8   8.8   6.9  88.0   -1.1    0.7   1.3 122.5  12.7 9999.000 999.000 999.0 999.0    78.0  3.0  2.0  2.0  4.0  4.0 99.0\n  12.0 1007.1   9.3   7.4  87.8   -1.2    1.0   1.6 129.8   6.5 9999.000 999.000 999.0 999.0   117.0  3.0  1.0  1.0  4.0  4.0 99.0\n  18.0 1003.2   9.2   7.4  88.2   -1.4    1.3   1.9 132.9   5.3 -122.200  37.700  74.1 115.0   149.0  1.0  1.0  1.0  4.0  4.0 99.0\n  24.0  999.2   8.9   7.3  89.5   -1.5    1.6   2.2 136.8   5.5 -122.200  37.700  88.6  83.9   182.0  1.0  1.0  1.0  4.0  4.0 99.0\n  30.0  995.1   8.6   7.2  91.2   -1.5    1.8   2.3 140.2   5.7 -122.200  37.700  79.2 125.5   216.0  1.0  1.0  1.0  4.0  4.0 99.0\n  36.0  991.4   8.4   7.3  92.6   -1.6    2.1   2.6 142.7   5.2 9999.000 999.000 999.0 999.0   247.0  1.0  1.0  1.0  4.0  4.0 99.0";
	protected static final String sounding2 = "Data Type:                         National Weather Service Sounding/Unspecified\nProject ID:                        0\nRelease Site Type/Site ID:         OAK Oakland, CA\nRelease Location (lon,lat,alt):    122 12.00'W, 37 42.00'N, -122.2,  37.7,    2.0\nUTC Release Time (y,m,d,h,m,s):    2006, 03, 01, 23:01:00\nAscension No:                      1154\nRadiosonde Serial Number:          84966634.CSN\nRadiosonde Manufacturer:           VIZ B2\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):2006, 03, 02, 00:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n   0.0 1017.1  14.7   5.2  53.0    3.9    3.3   5.1 229.8 999.0 -122.200  37.700 999.0 999.0     2.0  2.0  2.0  2.0  1.0  1.0  9.0\n   6.0 1009.3  13.0   4.8  57.4    3.6    3.3   4.9 227.5  10.8 9999.000 999.000 999.0 999.0    67.0  3.0  2.0  2.0  4.0  4.0 99.0\n  12.0 1005.6  12.5   4.4  57.7    3.3    3.3   4.7 225.0   5.2 9999.000 999.000 999.0 999.0    98.0  3.0  1.0  1.0  4.0  4.0 99.0\n  18.0 1002.2  12.2   4.1  57.8    2.9    3.3   4.4 221.3   4.7 -122.198  37.701  28.3 226.9   126.0  1.0  1.0  1.0  4.0  4.0 99.0\n  24.0  998.6  12.0   4.0  58.1    2.6    3.2   4.1 219.1   5.0 -122.198  37.702  29.2 227.5   156.0  1.0  1.0  1.0  4.0  4.0 99.0\n  30.0  994.0  11.7   4.0  59.0    2.3    3.2   3.9 215.7   6.5 -122.197  37.702  30.3 227.2   195.0  1.0  1.0  1.0  4.0  4.0 99.0";
	protected static final String sounding3 = "Data Type:                         National Weather Service Sounding/Unspecified\nProject ID:                        0\nRelease Site Type/Site ID:         NKX San Diego, CA\nRelease Location (lon,lat,alt):    117 06.00'W, 32 48.00'N, -117.1,  32.8,  134.0\nUTC Release Time (y,m,d,h,m,s):    2006, 03, 01, 11:25:00\nAscension No:                      1120\nRadiosonde Serial Number:          84969760.CSN\nRadiosonde Manufacturer:           VIZ B2\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):2006, 03, 01, 12:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n   0.0 1002.9   8.9   7.8  93.0   -0.7    0.7   1.0 135.0 999.0 -117.100  32.800 999.0 999.0   134.0 99.0 99.0 99.0 99.0 99.0  9.0\n   6.0  998.4   9.3   8.3  93.5   -0.6    0.5   0.8 129.8   6.2 9999.000 999.000 999.0 999.0   171.0 99.0 99.0  4.0  4.0  4.0 99.0\n  12.0  994.0   9.7   8.8  94.0   -0.5    0.3   0.6 121.0   6.2 9999.000 999.000 999.0 999.0   208.0 99.0 99.0  4.0  4.0  4.0 99.0\n  18.0  989.5  10.0   9.2  94.5   -0.4    0.1   0.4 104.0   6.3 9999.000 999.000 999.0 999.0   246.0 99.0 99.0  4.0  4.0  4.0 99.0\n  24.0  985.0  10.4   9.6  95.0   -0.3   -0.1   0.3  71.6   6.3 9999.000 999.000 999.0 999.0   284.0 99.0 99.0  4.0  4.0  4.0 99.0\n  30.0  982.2  10.2   9.5  95.5   -0.2   -0.3   0.4  33.7   3.8 9999.000 999.000 999.0 999.0   307.0 99.0 99.0 99.0  4.0  4.0 99.0";
	protected static final String sounding4 = "Data Type:                         National Weather Service Sounding/Unspecified\nProject ID:                        0\nRelease Site Type/Site ID:         NKX San Diego, CA\nRelease Location (lon,lat,alt):    117 06.00'W, 32 48.00'N, -117.1,  32.8,  134.0\nUTC Release Time (y,m,d,h,m,s):    2006, 03, 01, 23:20:00\nAscension No:                      1121\nRadiosonde Serial Number:          84969769.CSN\nRadiosonde Manufacturer:           VIZ B2\n/\n/\n/\nNominal Release Time (y,m,d,h,m,s):2006, 03, 02, 00:00:00\n Time  Press  Temp  Dewpt  RH    Ucmp   Vcmp   spd   dir   Wcmp     Lon     Lat   Ele   Azi    Alt    Qp   Qt   Qrh  Qu   Qv   QdZ\n  sec    mb     C     C     %     m/s    m/s   m/s   deg   m/s      deg     deg   deg   deg     m    code code code code code code\n------ ------ ----- ----- ----- ------ ------ ----- ----- ----- -------- ------- ----- ----- ------- ---- ---- ---- ---- ---- ----\n   0.0 1002.5  16.7   5.9  49.0    4.4   -1.6   4.7 290.0 999.0 -117.100  32.800 999.0 999.0   134.0  1.0  1.0  1.0  1.0  1.0  9.0\n   6.0  999.2  16.0   5.6  50.0    4.2   -1.4   4.4 288.4   4.7 9999.000 999.000 999.0 999.0   162.0  1.0  1.0  4.0  4.0  4.0 99.0\n  12.0  996.0  15.3   5.3  51.1    4.0   -1.2   4.2 286.7   4.5 9999.000 999.000 999.0 999.0   189.0  1.0  1.0  4.0  4.0  4.0 99.0\n  18.0  992.7  14.5   4.8  52.1    3.8   -1.0   3.9 284.7   4.7 9999.000 999.000 999.0 999.0   217.0  1.0  1.0  4.0  4.0  4.0 99.0\n  24.0  989.5  13.8   4.4  53.2    3.6   -0.9   3.7 284.0   4.7 9999.000 999.000 999.0 999.0   245.0  2.0  2.0  2.0  4.0  4.0 99.0\n  30.0  986.2  13.1   4.1  54.2    3.4   -0.7   3.5 281.6   4.7 -117.098  32.799  46.0 285.6   273.0  2.0  2.0  2.0  4.0  4.0 99.0";
}
