package dmg.ncepemc;

import java.io.*;
import java.util.*;

/**
 * The NcepMonthTester is a class that tests to make sure that the previous
 * month's ingest was not missed.
 *
 * @author Joel Clawson
 **/
public class NcepMonthTester {

    private int month, year;

    /**
     * Create a new instance of a NcepMonthTester.
     * @param month The month being processed.
     * @param year The year being processed.
     **/
    public NcepMonthTester(int month, int year) {
	this.month = month;
	this.year = year;
    }

    /**
     * Attempt to find the previous month tarball file.
     * @param dir The directory to be searched.
     * @return <code>true</code> if the file was found, <code>false</code>
     * if it was not.
     **/
    public boolean findPreviousMonthFile(String dir) {
	// Create a date to represent the current month.
	Calendar date = Calendar.getInstance();
	date.clear();
	date.set(Calendar.YEAR, year);
	date.set(Calendar.MONTH, month - 1);

	// Adjust the date to the previous month.
	date.add(Calendar.MONTH, -1);

	File file = null;
	int prevYear = date.get(Calendar.YEAR);

	// Define the previous month file.
	if (date.get(Calendar.MONTH) == 0) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_jan%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 1) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_feb%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 2) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_mar%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 3) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_apr%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 4) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_may%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 5) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_jun%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 6) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_jul%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 7) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_aug%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 8) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_sep%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 9) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_oct%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 10) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_nov%04d.tar",prevYear));
	} else if (date.get(Calendar.MONTH) == 11) {
	    file = new File(String.format("%s/%04d",dir,prevYear), 
			    String.format("katz_dec%04d.tar",prevYear));
	}

	// Try to find the file.
	return file != null && file.exists();
    }

    /**
     * Test for the previous month file.
     * @param args The current month, current year, and directory to be searched.
     **/
    public static void main(String[] args) {
	int month = Integer.parseInt(args[0]);
	int year = Integer.parseInt(args[1]);
	String dir = args[2];
	
	NcepMonthTester tester = new NcepMonthTester(month, year);

	if (!tester.findPreviousMonthFile(dir)) {
	    System.err.printf("The previous month file could not be found.\n");
	    System.exit(1);
	}
    }
}
