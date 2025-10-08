#!/usr/bin/env groovy
/**
 * This script was created to take NPRB FGDC metadata files from the PacMARS
 * project and insert our distribution information and remove any irrelevant
 * links to AOOS or ArcGIS.
 *
 * Author(s): Amanda Orin and Eric Dattore ~2013
**/

/* ******************************* IMPORTS ********************************* */
package arctic.xml

import groovy.xml.DOMBuilder;
import groovy.xml.dom.DOMCategory;
import groovy.xml.XmlUtil;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

//CliBuilder for command-line arguments
/* ***************************** END IMPORTS ******************************* */

/* REMINDER **************************************************************** *
 *   Be sure to capture all println calls into a single log file!
 * END REMINDER ************************************************************ */

/* ********************************* MAIN ********************************** */
String directory = '.'
String filename = ''

// Command Line Help
def cli = new CliBuilder(usage: 'profileProcessing.groovy [options] [file]', header: 'Options:')
cli.h('Show usage information')
cli.R('Process files recursively')
cli.D(args: 1, argName: 'directory', 'Specify an alternative directory path for processing')
// End of Command Line Help

// Grab the input options and parameters
def options = cli.parse(args)
if (!options) {
    return;
} else if (options.h) {
    cli.usage()
    return;
} else if (options.R) {
    // Do nothing for now. Implement recursion later.
}
def params = options.arguments();

if (options.D) {
    directory = options.D;
    directory = directory.replaceFirst(/[\/]*\s*$/, "");
}
filename = options.arguments()[options.arguments().size() - 1]

// Grab the file and save its contents as a String
print "\n  Retrieving ${directory}/${filename}..."

String xmlProfileContents = new File(directory + "/" + filename).text

println "  done.\n\n"

// If there are <?xml-stylesheet href='NBII_classic.xsl' type='text/xsl'?> elements, remove them
def stringToFind = '<?xml-stylesheet href=\'NBII_classic.xsl\' type=\'text/xsl\'?>';
if (xmlProfileContents.contains(stringToFind)) {
    def start = xmlProfileContents.indexOf(stringToFind);
    xmlProfileContents = xmlProfileContents.substring(0, start) + xmlProfileContents.substring(start + stringToFind.length() + 1);
}

// Parse the String of file contents with the XML parser
def reader = new StringReader(xmlProfileContents)
def doc = DOMBuilder.parse(reader)
def metadata = doc.documentElement

// Parse the String of cntinfo with the XML parser
def cntString = '''
<cntinfo>
	<cntorgp>
		<cntorg>National Center for Atmospheric Research</cntorg>
		<cntper>Earth Observing Laboratory</cntper>
	</cntorgp>
	<cntaddr>
		<addrtype>mailing and physical</addrtype>
		<address>3450 Mitchell Lane</address>
		<city>Boulder</city>
		<state>CO</state>
		<postal>80301</postal>
		<country>USA</country>
	</cntaddr>
	<cntvoice>303-497-8154</cntvoice>
	<cntemail>codiac@ucar.edu</cntemail>
	<cntemail>stott@ucar.edu</cntemail>
</cntinfo>
'''
def cntReader = new StringReader(cntString)
def cntDOM = DOMBuilder.parse(cntReader)
def cntRoot = cntDOM.documentElement

use(DOMCategory) {
    // Find all cntinfo elements whose immediate parent is the distinfo element
    def cntinfo = metadata.'**'.'distinfo'.'**'.'cntinfo'

    // If cntinfo didn't return any results, move on to the next replacement
    if (cntinfo.size() > 0) {
        // Replace it with the appropriate cntinfo element...
        cntinfo.each { ci ->
            // Remove the contents of the cntinfo tag...

            //println "${ci} \n"

            ci.'*'.each {
                it.getParentNode().removeChild(it)
            }

            // ... then add in the NCAR cntinfo tag's contents
            cntRoot.'*'.each { c ->
                //println "c:\t\t ${c.value()}"
                boolean isClass = ( c.getClass() == com.sun.org.apache.xerces.internal.dom.TextImpl )
                if ( isClass == false )
                    appendNodeDepth( ci, c )
            }

//            println "${ci} \n"
        }
    } else {
        println "  ${filename} does not contain any <cntinfo> elements within a <distinfo> element."
    }

    // Find all onlink elements that contain "www.aoos.org" or "http://www.nodc.noaa.gov/General/getdata.html"
    def allOnlinks = metadata.'**'.'onlink' // Get all onlink elements

    // If there are onlink elements...
    if (allOnlinks.size() > 0) {
        // Check if the contents contain the AOOS or NOAA data access URLs
        def matchedLinks = false
        allOnlinks.each { onlink ->
            if (onlink.text() == 'www.aoos.org') {
                matchedLinks = true;
                onlink.value = 'pacmars.eol.ucar.edu'
            } else if (onlink.text() == 'http://www.nodc.noaa.gov/General/getdata.html') {
                // If they do, remove that specific onlink element
                onlink.getParentNode().removeChild(onlink);
            }
        }

        if (!matchedLinks) {
            println "  ${filename} does not contain any links pertaining to NOAA or AOOS";
        }

    } else {
        println "  ${filename} does not contain any <onlink> elements.";
    }

    // If there are any <networkr> tags, replace the link with a PacMARS link
    def networkr = metadata.'**'.'networkr';
    if (networkr.size() > 0) {
        networkr.each { n ->
            if (n.text() == 'http://www.ArcGIS.com') {
                n.value = 'http://pacmars.eol.ucar.edu'
            }
        }
    }

    // If there are any references to anc-files.resdat.com, remove the section from <distInfo> through </distInfo>
    def ancFiles = metadata.'**'.'distInfo'.'**'.'linkage';
    if (ancFiles.size() > 0) {
        ancFiles.each {
            def nodeToRemove = it.getParentNode().getParentNode().getParentNode().getParentNode();
            nodeToRemove.getParentNode().removeChild(nodeToRemove);
        }
    }

    // If there are any references to offline media, remove them
    def offline = metadata.'**'.'offoptn';
    if (offline.size() > 0) {
        offline.each {
            it.getParentNode().removeChild(it);
        }
    }

}

def finalMetadata = XmlUtil.serialize(metadata);

def pattern = ~/(?m)\s{8}\n\s{8}\n\s{8}\n/;
finalMetadata = (finalMetadata =~ pattern).replaceAll("  ");

def outputFileName = filename.substring(filename.lastIndexOf('/'));
def fileOut = new File('output' + outputFileName).write(finalMetadata, 'UTF-8');

/* ******************************* END MAIN ******************************** */

/* ****************************** FUNCTIONS ******************************** */

def appendNodeDepth ( def appendToObject, def objectToAppend ) {
    //println " \"${objectToAppend.name()}\" "
    String objToAppText = objectToAppend.text()

    //println "${objectToAppend.'*'.size()}"
    if (objectToAppend.'*'.size() > 1) {
        objToAppText = ''
    }

    def appendedObject = appendToObject.appendNode( objectToAppend.name(), objToAppText )

    //println " \"${appendToObject}\" "
    objectToAppend.'*'.each { aoc ->
        //println "${aoc.getClass()}\n\t${aoc}"
        boolean isClass = ( aoc.getClass() == com.sun.org.apache.xerces.internal.dom.TextImpl || aoc.getClass() == com.sun.org.apache.xerces.internal.dom.DeferredTextImpl )
        //println "${isClass}:\t${aoc}"
        if ( isClass == false ) {
            //println "${aoc}"
            appendNodeDepth( appendedObject, aoc )
        }
    }
}
/* **************************** END FUNCTIONS ****************************** */
