/*
 * flaggedLine.java
 *
 * Created on November 1, 2007, 12:17 PM
 *
 */

package chatscrubber;

/**
 *
 * @author pmartin
 * Used to store the beginning and ending index of a line of text.
 * Also stores the length.
 */
public class flaggedLine implements Comparable{
    int start;
    int end;
    int length;
    /** Creates a new instance of flaggedLine */
    public flaggedLine(int inStart,int inEnd) {
        start = inStart;
        end = inEnd;
        length = end-start;
    }
    public int getEnd() {
        return end;
    }

    public int getLength() {
        return length;
    }

    public int getStart() {
        return start;
    }

    public void setEnd(int end) {
        this.end = end;
        length = end - start;
    }

    public void setLength(int length) {
        this.length = length;
        //update end for new length
        end = start+length;
    }

    public void setStart(int start) {
        this.start = start;
    }
    @Override
    public String toString(){
        return "Start: "+start+" Length: "+length;
    }
    //comparing the start of two lines
    //returns >0 if the passed object occurs earlier in the document
    //returns <0 if the passed object occurs later in the document
    //returns 0 if the two lines start at the same location
    public int compareTo(Object otherObject){ 
	  flaggedLine line2 = (flaggedLine)otherObject;
	  return start-line2.getStart();
    }
}
