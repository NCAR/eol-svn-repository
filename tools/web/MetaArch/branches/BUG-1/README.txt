--------------------------------------------------------------------------------
 About MetaArch
--------------------------------------------------------------------------------
  MetaArch is a data submission tool constructed for Arctic project use.
  Original concept based on data submission tool "bersea", and extended to 
  handle multiple projects with project-specific metadata and project-specific 
  look-and-feel for end users.
  
  Author: Amanda Orin
  
  Deployment URL:	https://data.eol.ucar.edu/metaarch/
  
  Status: Code reference, do not use unless bugs have been fixed!!!


--------------------------------------------------------------------------------
 Development Work
--------------------------------------------------------------------------------
 For the most detailed information of development work and status, refer to the 
 subversion logs.
 
 Latest development in-progress work includes:
 - revise index.gsp to allow for publicly viewable information and restricted 
   information for login-only users
 - file-batch-upload - how do we support a large quantity/volume of files?
   - multiple="multiple" allows for html multi-select
     - multiple attribute is not supported in IE 9 or earlier
   - how do we handle the filenames/sizes/formats if multi-select allows for 
     them all to be the same file type?
   - how to we support both the batch-uploader and the single-uploader?
     - SOLUTION: automatically direct view to batch-uploader, with link to 
	   single-uploader (i.e. URL parameter parsing for single-batch)
 - update css default colors to match EOL homepage colors - see eol.css
 
 
 Planned development work:
 - create and provide a log of overwriting dataset files viewable by DMG users
 - create a DMG approval step after archival-submission
   - this approval would auto-create a DTS entry for that dataset under that 
     dataset's project - project name mapping must be exact between MetaArch, 
     Zinc, and DTS
 - for internal contacts on projects:
   - receive e-mail notifications when datasets are created/updated/submitted 
     for given project
     - provide a daily email digest for changes? what level of notifications are 
       too many notifications?


 Debugging work:
 - For a more complete user experience depicting the scenarios for encountered 
   bugs in functionality, please see the MetaArch Testing Results PDF files.
 - Some views may be too-locked down for AJAX calls (e.g. authorship create 
   AJAX)
 - 



--------------------------------------------------------------------------------
 Requirements
--------------------------------------------------------------------------------
 Initial requirements include:
 - all user interaction requires the user to be logged in
 - all users need to be able to create (new) data sets
   - unless said user is not connected to any projects AND is not an 
     administrator
 - all users need to be able to view their own "profile"
 
 Additional requirements include:
 - file upload capability for data sets (single file or multiple)
 - project-specific metadata can be added as part of the data set submission 
   form
 - e-mail notifications for data submissions
 

 For a complete list of the requirements, please view the MetaArch Requirements
 word document.
 
--------------------------------------------------------------------------------
 Main Functionality Features
--------------------------------------------------------------------------------
- Project-specific metadata

- Project-specific look-and-feel

- E-mail notifications

- Creating new data sets by templating existing data sets
	