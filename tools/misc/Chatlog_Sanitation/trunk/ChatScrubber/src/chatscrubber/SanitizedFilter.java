/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package chatscrubber;

import java.io.File;
import java.io.FileFilter;

/**
 *
 * @author pmartin
 */
public class SanitizedFilter implements FileFilter{
    
    
    /**
     * Determines whether or not the file ends in the .san extension
     * @param f the File to check
     * @return true if the filename does not end with .san otherwise false
     */
    public boolean accept(File f) {
        if (f.getName().endsWith(".san")){
            return false;
        }
        else{
            return true;
        }
    }

}
