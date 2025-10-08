package meta

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

class TimeFormatService {

	def convertToUTC(Date date) {
		SimpleDateFormat sdf = new SimpleDateFormat ("yyyy-MM-dd HH:mm:ss");
		sdf.setTimeZone (TimeZone.getTimeZone ("UTC"));
		date = sdf.format(date);
		
		return date;
	}
	
	def convertToLocal(Date date, TimeZone timeZone) {
		SimpleDateFormat sdf = new SimpleDateFormat ("yyyy-MM-dd HH:mm:ss");
		sdf.setTimeZone (TimeZone.getTimeZone (timeZone));
		date = sdf.format(date);
		
		return date;
	}
	
	/*
    def serviceMethod() {
		Date now = new Date();
		SimpleDateFormat sdf = new SimpleDateFormat ("yyyy-MM-dd HH:mm:ss");
		sdf.setTimeZone (TimeZone.getTimeZone ("IST"));
		System.out.println ("Time in IST is " + sdf.format (now));
    }
    */
}
