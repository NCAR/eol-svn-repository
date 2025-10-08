/*
 * textChecker.java
 *
 * Created on October 23, 2007, 4:01 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package chatscrubber;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.event.UndoableEditEvent;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.UndoManager;

/**
 *
 * @author pmartin
 */
public class textChecker {
    //file name to be checked
    String file = null;
    //master document that is displayed
    StyledDocument doc;
    LinkedList flaggedLines;
    //file where regular expressions reside
    InputStream regexStream;
    //array of regular expressions
    ArrayList<String> regex;
    //matching stats
    int[] regexStats;
    //general sanitized statement
    String removed = "(Sanitized)";
    
    protected UndoManager undoManager = new UndoManager();
    
    SimpleAttributeSet highLighted = new SimpleAttributeSet();
    int index = 0;

    /** Creates a new instance of textChecker */
    public textChecker() {
        regex = new ArrayList();
        flaggedLines = new LinkedList();
        //set the default highlighted color scheme
        StyleConstants.setBackground(highLighted, Color.YELLOW);
        StyleConstants.setForeground(highLighted, Color.RED);
    }
    
    //input the regular expressions in the file into an array
    //assumes one regular expression per line
    //Accepts an inputStream instead of a File as input
    public boolean loadRegexFile(InputStream input) {
        regexStream = input;
        regexStream.mark(Integer.MAX_VALUE);
        regex = new ArrayList();
        String line = "";
        try {
            BufferedReader r = new BufferedReader(new InputStreamReader(regexStream));
            while (line != null) {
                if (line.length() != 0) {
                    regex.add(line);
                }
                line = r.readLine();
            }
            //adding our generic removed statement
            regex.add(removed);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
    
    //adds a exp to the list of regular expressions 
    public void addRegex(String exp) {
        regex.add(exp);
    }
    
    //insert the removed statement between the desired locations 
    //replaces the text between these locations and highlights the text that was inserted
    public void insert(int start, int end) {
        try {
            //remove selected text and insert new text
            undoManager.undoableEditHappened(new UndoableEditEvent(doc, new UndoableSanitation(doc, start, doc.getText(start, end - start))));
            doc.remove(start, end - start);
            doc.insertString(start, removed, doc.getStyle("Default"));
        } catch (BadLocationException ex) {
            ex.printStackTrace();
        }
        flaggedLine last = (flaggedLine) flaggedLines.get(flaggedLines.size() - 1);
        if (start >= last.getEnd()) {
            index = flaggedLines.size();
        } else {
            //set the index to highlight the inserted statement
            for (int i = 0; i < flaggedLines.size() - 1; i++) {

                flaggedLine line1 = (flaggedLine) flaggedLines.get(i);
                flaggedLine line2 = (flaggedLine) flaggedLines.get(i + 1);
                //check for a replacement of a flagged line
                if ((start <= line2.getStart()) && (end >= line2.getEnd())) {
                    index = i + 1;
                    flaggedLines.remove(index);
                    break;
                } else if ((start >= line1.getStart()) && (end <= line2.getStart())) {
                    index = i + 1;
                    break;
                }
            }
        }
        //reset
        markLines();
        unHighLightAll();
        highLight(index);
    }
    
    //save the changes made on the document to a new file appended with .san
    public boolean writeToFile(StyledDocument inDoc) {
        if (file != null) {
            File f = new File(file + ".san");
            BufferedWriter writer;
            try {
                writer = new BufferedWriter(new FileWriter(f));
                writer.write(inDoc.getText(0, inDoc.getLength()));
                writer.close();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
            return true;
        } else {
            return false;
        }
    }
    
    //replaces flaggedLine at current index with the generic removed statement
    //returns true if the flaggedLine was replaced correctly
    public boolean replace(StyledDocument inDoc) {
        if (flaggedLines.size() > 0) {
            flaggedLine curr = (flaggedLine) flaggedLines.get(index);
            try {
                undoManager.undoableEditHappened(new UndoableEditEvent(doc, new UndoableSanitation(doc, curr.start,doc.getText(curr.start, curr.length))));
                inDoc.remove(curr.start, curr.length);
                inDoc.insertString(curr.start, removed, inDoc.getStyle("Default"));
            } catch (BadLocationException ex) {
                ex.printStackTrace();
                return false;
            }
            return true;
        } else {
            return false;
        }
    }

    public boolean canUndo(){
        return undoManager.canUndo();
    }

    public boolean canRedo(){
        return undoManager.canRedo();
    }

    public void undo(){
        undoManager.undo();
        markLines();
        unHighLightAll();
        highLight(index);
    }

    public void redo(){
        undoManager.redo();
        markLines();
        unHighLightAll();
        highLight(index);
    }
    
    //reads the desired file and adds the text to the StyledDocument inDoc
    //only adds text where chatting occured throws an exception if no chatting occured
    public StyledDocument readFile(File f, StyledDocument inDoc) throws Exception {
        //reset undo and redo
        undoManager = new UndoManager();
        file = f.getPath();
        doc = inDoc;
        FileReader inputStream = null;
        flaggedLines = new LinkedList();
        index = 0;
        String text = "";
        try {
            //reset document
            doc.remove(0, doc.getLength());
            inputStream = new FileReader(f);
            BufferedReader reader = new BufferedReader(inputStream);
            String line = reader.readLine();
            //patterns used to keep certain kinds of text
            Pattern pattern1 = Pattern.compile("<.+>");
            Pattern pattern2 = Pattern.compile("^\\*{4}"); //4 * signifying beginning or ending of logging
            Pattern pattern3 = Pattern.compile("---"); //--- signifying user change
            ArrayList<Pattern> patterns = new ArrayList();
            patterns.add(pattern1);
            patterns.add(pattern2);
            patterns.add(pattern3);
            while (line != null) {
                //check for actual chatting not just users entering and leaving
                //Also check for beginning and ending of logging and user name changes
                //All chat exchanges have "<nick>" at the beginning, where "nick" is the nickname of the person chatting.
                for (Pattern p : patterns) {
                    Matcher matcher = p.matcher(line);
                    if (matcher.find()) {
                        text += line + "\n";
                        break;
                    }
                }
                line = reader.readLine();
            }
            Style style = doc.addStyle("Default", null);
            doc.insertString(doc.getLength(), text, style);
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (text.length() == 0) {
            throw new Exception("No chatting occured.");
        }
        return doc;
    }
    
    //checks text against every regular expression
    public int markLines() {
        //create the marked attributes
        SimpleAttributeSet marked = new SimpleAttributeSet();
        StyleConstants.setForeground(marked, Color.RED);
        //reset the arraylist containing the marked lines
        Iterator iter = flaggedLines.iterator();
        while (iter.hasNext()) {
            iter.next();
            iter.remove();
        }
        //set the stats array
        regexStats = new int[regex.size()];
        try {
            String text = doc.getText(0, doc.getLength());
            //go through the array of regular expressions and create flaggedlines of matched text
            for (int i = 0; i < regex.size(); i++) {
                String reg = (String) regex.get(i);

                //ignore case and diseregard comments denoted with "#"
                Pattern pattern = Pattern.compile(reg, Pattern.CASE_INSENSITIVE | Pattern.COMMENTS);

                Matcher matcher = pattern.matcher(text);
                //make sure we do not match empty text
                Matcher blankTest = pattern.matcher("");
                if (!blankTest.matches()) {
                    while (matcher.find()) {
                        orderedInsert(new flaggedLine(matcher.start(), matcher.end()));
                        regexStats[i]++;
                    }
                }
            }
            //set each flaggedLine to be marked
            for (int i = 0; i < flaggedLines.size(); i++) {
                flaggedLine line = (flaggedLine) flaggedLines.get(i);
                doc.setCharacterAttributes(line.start, line.length, marked, false);
            }
        } catch (BadLocationException ex) {
            ex.printStackTrace();
        }
        return flaggedLines.size();
    }
    
    //used to insert the flaggedLines into the linkedList in the correct order based on location in the text
    public void orderedInsert(Object value) {
        ListIterator iter = flaggedLines.listIterator();
        Comparable comp = (Comparable) value;
        boolean add = true;
        while (iter.hasNext()) {
            Comparable listComp = (Comparable) iter.next();
            //make sure we do not add two of the same flagged lines
            if (comp.compareTo(listComp) == 0) {
                add = false;
                break;
            } else if (comp.compareTo(listComp) < 0) {
                iter.previous();
                add = true;
                break;
            }
        }
        if (add) {
            iter.add(value);
        }
    }

    //highlight the next flagged line at the next index
    //returns the position of the flagged line
    //throws an exception if there are no flagged lines or if the end of the flagged lines was reached
    public int getNext() throws Exception {
        if (flaggedLines.size() == 0) {
            throw new Exception("No flagged lines");
        } else if (index >= flaggedLines.size() - 1) {
            throw new Exception("End of flagged lines");
        } else {
            flaggedLine next = (flaggedLine) flaggedLines.get(++index);
            unHighLightAll();
            highLight(index);
            return next.getStart();
        }
    }
    
    //highlight the previous flagged line at the previousindex
    //returns position of flaggedLine
    //throws an exception if no flagged lines exist
    public int getBack() throws Exception {
        int pos = 0;
        if (flaggedLines.size() == 0) {
            throw new Exception("No flagged lines");
        } else {
            if (index != 0) {
                flaggedLine back = (flaggedLine) flaggedLines.get(--index);
                pos = back.getStart();
            } else {
                flaggedLine current = (flaggedLine) flaggedLines.get(index);
                pos = current.getStart();
            }
            unHighLightAll();
            highLight(index);
            return pos;
        }
    }
    
    //used to return a two dimensional array filled with regular expressions and how many times they were matched
    public Object[][] getStatData() {
        Object[][] data = new Object[regex.size() + 1][2];
        int count = 0;
        //keep track of total matching lines
        int total = 0;
        for (int i = 0; i < regex.size(); i++) {
            //only show regular expressions that were actually matched
            if (regexStats[i] > 0) {
                data[count][0] = regex.get(i);
                data[count][1] = regexStats[i];
                total += regexStats[i];
                count++;
            }
        }
        //add a blank line 
        count++;
        data[count][0] = new String("Total");
        data[count][1] = total;
        return data;
    }
    
    //highlight a flagggedLine based on index
    public void highLight(int i) {
        flaggedLine currentLine = (flaggedLine) flaggedLines.get(i);
        doc.setCharacterAttributes(currentLine.getStart(), currentLine.getLength(), highLighted, false);
    }
    
    //unHighlight a flaggedLine based on index
    public void unHighLight(int i) {
        SimpleAttributeSet unHighLighted = new SimpleAttributeSet();
        StyleConstants.setForeground(unHighLighted, Color.RED);
        flaggedLine currentLine = (flaggedLine) flaggedLines.get(i);
        doc.setCharacterAttributes(currentLine.getStart(), currentLine.getLength(), unHighLighted, true);
    }
    
    //unhighlight all flagged lines
    public void unHighLightAll() {
        SimpleAttributeSet unHighLighted = new SimpleAttributeSet();
        StyleConstants.setForeground(unHighLighted, Color.RED);
        for (int i = 0; i < flaggedLines.size(); i++) {
            flaggedLine currentLine = (flaggedLine) flaggedLines.get(i);
            doc.setCharacterAttributes(currentLine.getStart(), currentLine.getLength(), unHighLighted, true);
        }
    }
    
    //highLight current line
    //returns position of flaggedLine
    public int getCurrent() {
        highLight(index);
        if (index > 0) {
            unHighLight(index - 1);
        }
        if (index < flaggedLines.size() - 1) {
            unHighLight(index + 1);
        }
        flaggedLine line = (flaggedLine) flaggedLines.get(index);
        int pos = line.getStart();
        return pos;

    }
    
    //return the number of regular expressions
    public int getNumRegex() {
        return regex.size();
    }
    
    //for debugging purposes
    private String printList(LinkedList a) {
        Iterator iter = a.iterator();
        String text = "";
        while (iter.hasNext()) {
            text += iter.next().toString() + "\n";
        }
        return text;
    }
    
    //returns a string of the regular expresions
    public String printRegex() {
        String text = "";
        for (int i = 0; i <= regex.size() - 1; i++) {
            text += regex.get(i).toString() + "\n";
        }
        return text;
    }
    
    //returns current file being processed 
    public File getFile() {
        return new File(file);
    }

    /**UNDO SECTION************************************************************/
    class UndoableSanitation extends AbstractUndoableEdit {

        protected StyledDocument document;
        //keep track of position of line that was sanitized
        protected int index;
        //keep track of original text before sanitation 
        protected String text;

        public UndoableSanitation(StyledDocument inDoc, int start, String txt) {
            document = inDoc;
            index = start;
            text = txt;
        }

        public String getPresentationName() {
            return "Sanitation";
        }

        public void undo() {
            super.undo();
            try {
                //remove sanitized statement
                document.remove(index, removed.length());
                //insert old text
                document.insertString(index, text, document.getStyle("Default"));
            } catch (BadLocationException ex) {
                Logger.getLogger(textChecker.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        public void redo() {
            super.redo();
            try {
                //remove text
                document.remove(index,text.length());
                //replace with sanitized statement
                document.insertString(index, removed, document.getStyle("Default"));
            } catch (BadLocationException ex) {
                Logger.getLogger(textChecker.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }
}
