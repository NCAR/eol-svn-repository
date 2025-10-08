package eventupdate;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * The <code>Event</code> class is used to keep track of the beginning and end dates of an event, as well as the name.
 * @author pmartin
 */
public class Event implements Comparable {

    /** The date of the start of the event. */
    public Date begin;
    /** The date of the ending of the event. */
    public Date end;
    /**The name of the event. */
    public String event;

    /**
     * Constructs a new Event object.
     * @param beginning The start date
     * @param ending The end date
     * @param name The title of the event
     */
    public Event(Date beginning, Date ending, String name) {
        begin = beginning;
        end = ending;
        event = name;
    }

    /**
     * Creates a <code>String</code> representation of this event.
     * @return a <code>String</code> in the form <i>Event Begin End</i>.
     */
    @Override
    public String toString() {
        return event + ": " + begin + " -> " + end;
    }
    
    /**
     * Gets the begin date of the event
     * @return a String of the date in the form yyyy-MM-dd HH:mm:ss
     */
    public String getBegin(){
        DateFormat dfm = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        return dfm.format(begin);
    }

    /**
     * Gets the end date of the event
     * @return a String of the date in the form yyyy-MM-dd HH:mm:ss
     */
    public String getEnd(){
        DateFormat dfm = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        return dfm.format(end);
    }

    /**
     * Compares this event to another event.
     * If the start date and the end date are the same then it is the same.
     * Otherwise it compares the start date.
     * @param other the Object to compare with
     * @return 0 if the events have the same start and end date. 
     * If the start date of this event comes after the other start date than an int greater the 0 is returned.
     * If the start date of this event comes before the other start date then an int less than 0 is returned.
     */
    @Override
    public int compareTo(Object other) {
        Event otherEV = (Event) other;
        if ((this.begin.compareTo(otherEV.begin) == 0) && (this.end.compareTo(otherEV.end) == 0)) {
            return 0;
        } else {
            return this.begin.compareTo(otherEV.begin);
        }
    }
}
