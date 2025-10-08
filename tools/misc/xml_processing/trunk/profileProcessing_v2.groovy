#!/usr/bin/env groovy
/**
 * This script was created to take NPRB FGDC metadata files from the PacMARS
 * project and insert our distribution information and remove any irrelevant
 * links to AOOS or ArcGIS.
 *
 * This is a revision to the original script to make it easier to use. It still
 * behaves in a similar way.
 *
 * Author(s): Eric Dattore ~2014
**/
package arctic.xml

import groovy.xml.XmlUtil

String directory = '.';
String filename = '';

def cli = new CliBuilder(usage: 'profileProcessing.groovy [options] [file]', header: 'Options');
cli.h('Show usage information');
cli.R('Process files recursively');
cli.D(args: 1, argName: 'directory', 'Specify an alternative directory path for processing');

def options = cli.parse(args);
if (!options) {
    return;
} else if (options.h) {
    cli.usage();
    return;
} else if (options.R) {
    // Implement later
}

def params = options.arguments();

if (options.D) {
    directory = options.D;
    directory = director.replaceFirst(/[\/]*\s*$/, "");
}

filename = options.arguments()[options.arguments().size() - 1];

print "\n Retrieving ${directory}/${filename}...";

String xmlProfileContents = new File(directory + "/" + filename).text;

println " done.\n\n";

def root = new XmlParser().parseText(xmlProfileContents);

// Find the cntinfo tags and replace them
def cntinfo = root.depthFirst().find { it.name() == 'distinfo'; }?.depthFirst().find { it.name() == 'cntinfo' };

if (!cntinfo) {
    cntinfo = root.appendNode('distinfo').appendNode('distrib').appendNode('cntinfo');
} else {
    def parent = cntinfo.parent();
    parent.remove(cntinfo);
    cntinfo = parent.appendNode('cntinfo');
}

def cntorgp = cntinfo.appendNode('cntorgp');
cntorgp.appendNode('cntorg', null, 'National Center for Atmospheric Research');
cntorgp.appendNode('cntper', null, 'Earth Observing Laboratory');
def cntaddr = cntinfo.appendNode('cntaddr');
cntaddr.appendNode('addrtype', null, 'mailing and physical');
cntaddr.appendNode('address', null, '3450 Mitchell Lane');
cntaddr.appendNode('city', null, 'Boulder');
cntaddr.appendNode('state', null, 'CO');
cntaddr.appendNode('postal', null, '80301');
cntaddr.appendNode('country', null, 'USA');
cntinfo.appendNode('cntvoice', null, '303-497-8154');
cntinfo.appendNode('cntemail', null, 'codiac@ucar.edu');
cntinfo.appendNode('cntemail', null, 'stott@ucar.edu');

// Find all onlinks that refer to aoss and replace them with a reference to PacMARS
def onlinks = root.depthFirst().findAll { it.getClass() == Node && it.name() == 'onlink' };

if (onlinks) {
    onlinks.each {
        if (it.text() == 'www.aoos.org') {
            it.value = 'pacmars.eol.ucar.edu';
        }
        if (it.text() == 'http://www.nodc.noaa.gov/General/getdata.html') {
            it.parent().remove(it);
        }
    }
} else {
    println "No onlink tags found";
}

def networkr = root.depthFirst().findAll { it.getClass() == Node && it.name() == 'networkr' };
if (networkr) {
    networkr.each {
        if (it.text() == 'http://www.ArcGIS.com') {
            it.value = 'http://pacmars.eol.ucar.edu';
        }
    }
} else {
    println "No networkr tags found";
}

// Remove all tags corresponding to an offline ordering mode
def offoptn = root.depthFirst().findAll { it.getClass() == Node && it.name() == 'offoptn' };
if (offoptn) {
    offoptn.each {
        it.parent().remove(it);
    }
} else {
    println "No offoptn tags found";
}

// Remove distinfo past Esri tags
def esriDistinfo = root.depthFirst().findAll { it.getClass() == Node && it.name() == 'linkage' };

if (esriDistinfo) {
    esriDistinfo.each {
        it.parent().parent().parent().parent().parent().remove(it.parent().parent().parent().parent());
    }
} else {
    println "No Esri Distinfo tags found";
}

def finalMetadata = XmlUtil.serialize(root);
//println finalMetadata;
def outputFileName = filename.substring(filename.lastIndexOf('/'));
def fileOut = new File('output' + outputFileName).write(finalMetadata, 'UTF-8');
