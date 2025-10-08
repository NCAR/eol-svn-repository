package eventupdate;



import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;

/**
 * The <code>EventUpdate</code> class is used to update files in a MySQL database.
 * Files can be tagged with event strings if they match certain date criteria.
 * @author pmartin
 */
public class EventUpdate {

    /**
     * The Database object that is used to connect to the actual MySQL database
     */
    Database db;
    /**
     * keeps track of the status of the connection with the database
     */
    boolean connection = true;
    String logFile = "";
    String logFilePath;

    /**
     * Connects to the database and determines if the connection was successful.
     * @param Host the address of the MySQL host database
     * @param Db the name of the database to use
     * @param User the username to be used in the connection
     * @param Pass the password for the user
     * @param filePath The location of the logFile to write error messages
     */
    public EventUpdate(String Host, String Db, String User, String Pass, String filePath) {
        logFilePath = filePath;
        try {
            db = new Database(Host, Db, User, Pass);
        } catch (SQLException ex) {
            connection = false;
            print("\nSQLException: " + ex.getMessage() + "\n");
        }
    }

    /**
     * Gets all of the files and their properties from a specific dataset.
     * The attributes that are selected are filename,dataset_id,begin_date,end_date,event,purpose
     * @param dataset_id the dataset from which to retrieve the files
     * @return an <code>ArrayList</code> containing all of the files
     */
    public ArrayList getFiles(double dataset_id) {
        ArrayList files;
        try {
            //get file information for the specified dataset
            files = db.select("filename,dataset_id,begin_date,end_date,event,purpose", "file", "dataset_id = " + dataset_id);
        } catch (SQLException ex) {
            print(ex.getMessage());
            files = null;
        }
        if (files.size() == 0) {
            print("0 Files were found for dataset " + dataset_id);
        }
        return files;
    }

    /**
     * Prints the files from the specified dataset.
     * Used for debugging
     * @param dataset_id the dataset to print
     */
    private void printFiles(double dataset_id) {
        try {
            print(db.printSelect("filename,dataset_id,begin_date,end_date,event", "file", "dataset_id = " + dataset_id));
        } catch (SQLException ex) {
            print(ex.getMessage());
        }
    }

    /**
     * Updates the begin date and the end date of each file if it matches an event
     * If the file matches an event the file will be updated to have the begin and end date of this event
     * Checks every file for the specified project.
     * @param events the ArrayList containing the events that will be checked
     * @param project_id the number of the project to be checked 
     * @return the number of files that were updated
     * @see Event
     */
    public int updateProjectbyEvent(ArrayList<Event> events, int project_id) {
        ArrayList<HashMap> datasets = this.getDatasets(project_id);

        //keeps track of how many files are updated
        int total = 0;

        //loops through all of the datasets
        for (HashMap dataset_ids : datasets) {

            //Gets specific dataset_id
            Double dataset_id = Double.parseDouble(dataset_ids.get("dataset_id").toString());
            print("\nProcessing: " + dataset_id);

            //tags each individual dataset
            int num = updateDatasetbyEvent(events, dataset_id);
            total += num;
            print("\nDataset: " + dataset_id + " had " + num + " files that had new begin date and end dates\n");
        }
        return total;
    }

    /**
     * Updates the begin date and the end date of each file if it matches an event
     * If the file matches an event the file will be updated to have the begin and end date of this event
     * @param events the ArrayList containing the events that will be checked
     * @param dataset_id the number of the dataset to be updated
     * @return the number of files that were updated
     * @see Event
     */
    public int updateDatasetbyEvent(ArrayList<Event> events, double dataset_id) {
        //get all of the files for this dataset
        ArrayList<HashMap> files = getFiles(dataset_id);

        //keeps track of how many files are tagged
        int num = 0;

        //loops through each file
        for (HashMap file : files) {
            //only modify data files
            if (file.get("purpose").equals("data")) {
                //loops through each event
                print("Checking: " + file.get("filename") + " event: " + file.get("event"));
                for (Event e : events) {
                    if (e.event.equalsIgnoreCase((String) file.get("event"))) {
                        try {
                            print(file.get("filename") + " now has begin and end dates for event " + e.toString());
                            num += db.update("file", "begin_date = '" + e.getBegin() + "' , end_date = '" + e.getEnd() + "'", "filename = \"" + file.get("filename") + "\" and dataset_id = " + "\"" + file.get("dataset_id") + "\"");
                        } catch (SQLException ex) {
                            print("\n\nERROR UPDATING BEGIN AND END DATES!!!");
                            print(ex.getMessage());
                            //rollback the database if any errors occur
                            try {
                                db.rollback();
                            } catch (SQLException ex1) {
                                print("\nERROR ROLLING BACK UPDATES!!!");
                                print(ex1.getMessage());
                            }
                        }
                    }
                }
            }
        }
        return num;
    }

    /**
     * Tags files within a certain dataset with an event string if they fall within the BEGIN and END dates
     * Multiple events can be checked.
     * @param events the ArrayList containing the events that will be checked
     * @param dataset_id the number of the dataset to be tagged
     * @return the number of files that were tagged
     * @see Event
     */
    public int tagDatasetbyDate(ArrayList<Event> events, double dataset_id) {
        //get all of the files for this dataset
        ArrayList<HashMap> files = getFiles(dataset_id);

        //keeps track of how many files are tagged
        int num = 0;

        //loops through each file
        for (HashMap file : files) {

            if (file.get("purpose").equals("data")) {

                //makes sure we do not change an event that has already been defined
                if (file.get("event") == null || file.get("event").equals(new String(""))) {
                    //begin and end date of the file
                    Date fileBegin = (Date) file.get("begin_date");
                    Date fileEnd = (Date) file.get("end_date");
                    //loops through each event

                    for (Event e : events) {
                        Date begin = e.begin;
                        Date end = e.end;
                        String event = e.event;
                        try {
                            //checks if the file falls withing the begin and end dates of the event
                            if (fileBegin.after(begin) && fileEnd.before(end)) {

                                //update the event for this file
                                num += db.update("file", "event = \"" + event + "\"", "filename = \"" + file.get("filename") + "\" and dataset_id = " + "\"" + file.get("dataset_id") + "\"");
                                print(file.get("filename") + ": " + fileBegin.toString() + " -> " + fileEnd.toString() + " tagged with " + event);

                            } //checks if the file spans the entire event
                            else if (fileBegin.before(begin) && fileEnd.after(end)) {
                                print("***WARNING*** FILE: " + file.get("filename") + " spans the entire event " + event);
                            } //checks if the file either begins or ends inside the event timeframe
                            else if ((fileBegin.after(begin) && fileBegin.before(end)) || (fileEnd.after(begin) && fileEnd.before(end))) {
                                print("***WARNING*** FILE: " + file.get("filename") + " has a begin or end date within the event " + event);
                                print(file.get("filename") + ": " + fileBegin.toString() + " -> " + fileEnd.toString() + " tagged with " + event);
                                num += db.update("file", "event = \"" + event + "\"", "filename = \"" + file.get("filename") + "\" and dataset_id = " + "\"" + file.get("dataset_id") + "\"");
                            }
                        } catch (SQLException ex) {
                            print("\n\nERROR UPDATING EVENT!!!");
                            print(ex.getMessage());
                            //rollback the database if any errors occur
                            try {
                                db.rollback();
                            } catch (SQLException ex1) {
                                print("\nERROR ROLLING BACK UPDATES!!!");
                                print(ex1.getMessage());
                            }
                            return num;
                        }
                    }
                } else {
                    print("***WARNING*** EVENT IS ALREADY SET FOR: " + file.get("filename") + " WITH " + file.get("event") + " NOT CHANGING");
                }
            } else {
                print("***WARNING*** FILETYPE FOR: " + file.get("filename") + " IS NOT DATA. NOT CHANGING EVENT");
            }
        }
        return num;
    }

    /**
     * Tags every dataset in a project with an event string if they match the date criteria.
     * Every file in each dataset will be analyzed.  
     * @param events the ArrayList containing the events that will be checked
     * @param project_id the id of the project to be tagged.
     * @return the number of files that were tagged
     * @see Event
     */
    public int tagProjectbyDate(ArrayList<Event> events, int project_id) {
        ArrayList<HashMap> datasets = this.getDatasets(project_id);

        //keeps track of how many files are updated
        int total = 0;

        //loops through all of the datasets
        for (HashMap dataset_ids : datasets) {

            //Gets specific dataset_id
            Double dataset_id = Double.parseDouble(dataset_ids.get("dataset_id").toString());
            print("\nProcessing: " + dataset_id);

            //tags each individual dataset
            int num = tagDatasetbyDate(events, dataset_id);
            total += num;
            print("\nDataset: " + dataset_id + " had " + num + " files that were updated with an event\n");
        }
        return total;
    }

    /**
     * Gets all of the datasets for the specified project id
     * @param project_id the id of the project
     * @return a HashMap of all of the datasets that match this project id
     */
    public ArrayList<HashMap> getDatasets(int project_id) {
        ArrayList<HashMap> datasets = null;
        try {
            //gets a list of all of the datasets in a specific project
            datasets = db.select("dataset_id", "dataset", "dataset_id like \"" + project_id + ".%\"");
            if (datasets.size() == 0) {
                System.out.println("No datasets were found for project" + project_id);
            }
        } catch (SQLException ex) {
            print(ex.getMessage());
        }
        return datasets;
    }

    /**
     * Displays a prompt to the user and captures the input
     * @param message the String to be displayed
     * @return a String containing the input
     */
    public static String getUserInput(String message) {
        //  prompt the user to enter input
        System.out.print(message);

        //  open up standard input
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        String input = null;

        //  read the input from the command-line; need to use try/catch with the
        //  readLine() method
        try {
            input = br.readLine();
        } catch (IOException ex) {
            System.out.println("\nIO ERROR");
            System.out.println(ex.getMessage());
        }

        return input;
    }

    /**
     * Appends a String to the specified file.  Writes the message followed by a "\n" character
     * @param message the String to be appended to the file
     * @param filename the String location of the file
     */
    public void writeToFile(String message, String filename) {
        try {
            BufferedWriter out = new BufferedWriter(new FileWriter(filename, true));
            out.write(message + "\n");
            out.close();
        } catch (IOException e) {
            print("\nERROR writing to logfile");
            print(e.getMessage());
        }
    }

    /**
     * Displays a message to the standard output as well as to a log file.  The file is named logFile and will be appended.
     * @param message the String to be displayed and recorded.  
     */
    public void print(String message) {
        writeToFile(message, logFilePath);
        System.out.println(message);
    }

    /**
     * Extracts event information from a file
     * The file should be in the format: Event Start_Date End_Date.
     * where the start and end dates are in the form yyyy-MM-dd HH:mm:ss
     * One event should be specified per line.
     * ex: Wilma 2005-10-16 00:00:00 2005-10-25 23:59:59
     * @param filePath the location of the file to be read
     * @return an ArrayList containing all of the events
     */
    public ArrayList<Event> loadEvents(String filePath) {
        String fileContents = this.loadFile(filePath);

        ArrayList<Event> events = null;

        if (fileContents != null) {
            events = new ArrayList();
            //gets each line
            String[] lines = fileContents.split("\n");

            for (int i = 0; i < lines.length; i++) {
                //gets each word
                String[] words = lines[i].split("\\s");
                DateFormat dfm = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                Date begin, end;
                try {
                    //gets each date
                    begin = dfm.parse(words[1] + " " + words[2]);
                    end = dfm.parse(words[3] + " " + words[4]);
                    //adds this event to the Arraylist
                    events.add(new Event(begin, end, words[0]));
                } catch (Exception ex) {
                    print("ERROR: " + ex.getMessage());
                    return null;
                }
            }
        }
        return events;
    }

    /**
     * Asks the user if they would like to commit the changes
     * @param numUpdates the number of changes to display to the user
     * @return true if the user decided to commit the changes otherwise false
     */
    public boolean userCommit(int numUpdates) {
        print("\n\n" + numUpdates + " Files are ready to be updated.");
        //asks the user to choose whether to commit or rollback the changes
        String input = getUserInput("To commit the changes type \"y\" and press enter.\nOtherwise enter any value and press enter to cancel" +
                ".\n\n>>");

        //commit
        if (input.equalsIgnoreCase("y")) {
            try {
                db.commit();
                print("\nUser decided to commit\n" + numUpdates + " Files were updated");
            } catch (SQLException ex) {
                print("\n\nERROR PERFORMING COMMIT");
                print(ex.getMessage());
            }
            return true;
        } //rollback
        else {
            try {
                db.rollback();
                print("DATABASE ROLLEDBACK");
            } catch (SQLException ex1) {
                print("\nERROR ROLLING BACK UPDATES!!!");
                print(ex1.getMessage());
            }
            return false;
        }
    }

    /**
     * Reads each line from the specified file.
     * @param filepath the location and name of the file
     * @return a <code>String</code> containing the results with lines separated by \n characters
     */
    public String loadFile(String filepath) {
        String output = "";
        FileInputStream f = null;
        try {
            //open up file
            f = new FileInputStream(filepath);
        } catch (FileNotFoundException ex) {
            print("ERROR: " + ex.getMessage());
            return null;
        }
        if (f != null) {
            //Read this lines from the file
            BufferedReader reader = new BufferedReader(new InputStreamReader(f));
            try {
                String line = reader.readLine();
                while (line != null) {
                    //gets each line
                    output += line + "\n";
                    line = reader.readLine();
                }
                f.close();
            } catch (IOException ex) {
                print(ex.getMessage());
            }
        }
        return output;
    }

    /**
     * Performs the event update.
     * The event file is used to specify the events and the begin and end dates of each event
     * @param args input arguments. The order of input is: <code>Host Database User Password Project_id Event_File</code>
     */
    public static void main(String[] args) {

        //creates a log file with date information in the name
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat sdf = new SimpleDateFormat("ddMMMyyyy-HH:mm:ss");
        String time = sdf.format(cal.getTime());

        String filePath = "log-" + time;
        System.out.println("Log file is: "+filePath);
        File f = new File(filePath);
        System.out.println(f.getAbsoluteFile());


        //gets information for database connection
        String host = EventUpdate.getUserInput("\nEnter the host locatation of the database-> ");
        String database = EventUpdate.getUserInput("Enter the database name-> ");
        String username = EventUpdate.getUserInput("Enter the username-> ");
        String password = EventUpdate.getUserInput("Enter the password-> ");

        //gest the location of the event file to be read
        String path = EventUpdate.getUserInput("Enter event file-> ");

        //creates an instance of the EventUpdate class and tries to connect to the database
        EventUpdate update = new EventUpdate(host, database, username, password,filePath);

        //load the events from the event file
        ArrayList<Event> events = update.loadEvents(path);

        //make sure the connection was successful and that the event file was loaded
        if (update.connection == true && events != null) {

            //keep track of the number of updates
            int total = 0;

            String id = EventUpdate.getUserInput("Enter the project_id or dataset_id-> ");

            int opt = 0;

            do {
                String option = EventUpdate.getUserInput("\nEnter 1 to tag files with events by date.\nEnter 2 to tag files with dates by events\n\n-> ");
                opt = Integer.parseInt(option);
            } while (!(opt == 1 || opt == 2));

            switch (opt) {
                case 1:
                    update.print("Tagging all files for "+id+" with an event if they fall within the begin or end date of an event specified in the event file: "+path+"\n");
                    //check for dataset otherwise it is a project
                    if (id.contains(".")) {
                        Double dataset_id = Double.parseDouble(id);
                        total = update.tagDatasetbyDate(events, dataset_id);
                    } else {
                        int project_id = Integer.parseInt(id);
                        total = update.tagProjectbyDate(events, project_id);
                    }
                    break;

                case 2:
                    update.print("Updating dates for files in "+id+"\nAll files that match an event specified in the event file: "+path+" will now have the same begin and end date as the matching event.\n");
                    //check for dataset otherwise it is a project
                    if (id.contains(".")) {
                        Double dataset_id = Double.parseDouble(id);
                        total = update.updateDatasetbyEvent(events, dataset_id);
                    } else {
                        int project_id = Integer.parseInt(id);
                        total = update.updateProjectbyEvent(events, project_id);
                    }
                    break;
            }
            //asks the user whether to commit the changes
            update.userCommit(total);
        }
    }
}
