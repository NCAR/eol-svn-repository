--------------------------------------------------------------------------------
SEARCH UI USING JAVASCRIPT, AJAX, AND CGI

  Date: May 8, 2014
  Author: Amanda Orin
--------------------------------------------------------------------------------

This file is designed to help you understand the concepts behind how the search 
functionality of the Bering Sea Project / PacMARS data access user interface 
was designed and implemented.


JavaScript Files:
  - bootstrap.min.js -- the Twitter Bootstrap JavaScript library
  - bindable.js -- makes the results table columns alphabetically sortable
  - main.js -- contains the main functions that build the AJAX call URL
  - styles.js

CSS Files:
  - bootstrap.css -- the Twitter Bootstrap CSS library
  - main.css -- contains the styles for all the search and results
  - main-ie.css -- contains Internet Explorer specific styles

CGI Files:
  - See Master List Viewer for CGI files called.
    http://svn.eol.ucar.edu/svn/dmg/tools/web/master_list_viewer/trunk/cgi-bin/master_query
  
HTML Files:
  - index.html -- contains the form (name = "dta_form","dta_form_xs") and 
                  results table (id = "r_content") elements


This specific version of the data access search UI was built with responsive 
design in mind. This means that, technically, there are two versions of the form
that can be used, depending upon the width of your browser and/or device. See 
the JavaScript snippet in index.html in the "r_content" element for the 
initialization of the form and results (i.e. reset_opt() triggers the results
at start).

The functions in main.js that do the heavy lifting for this search interface 
are as follows:
  - check_opts()      -- triggered when project checkboxes are turned on/off
                         triggers set_opts()
  - set_opts()        -- triggered by check_opts() to enable/disable select 
                         options in cruise/subject portions of the form
  - clear_list()      -- triggered when "Clear" button is clicked in UI; 
                         de-selects all current selections
  - build_results()   -- processes the form selections to create an AJAX URL to 
                         call the CGI file that constructs the HTML results; 
                         triggers strip_results(), build_query()
  - strip_results()   -- triggered by build_results() to replace whitespace for 
                         URL use
  - build_query()     -- triggered by build_results() to construct the paragraph
                         detailing what the results are specifically selected 
                         from (i.e. project, cruise, subject, etc.)