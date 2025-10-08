--------------------------------------------------------------------------------
SEARCH UI USING JSON FILE

  Date: May 7, 2014
  Author: Amanda Orin
--------------------------------------------------------------------------------

This file is designed to help you understand the concepts behind how the search 
functionality of the DBO data access user interface was designed and 
implemented.


JavaScript Files:
  - data.js -- contains the primary functionality for searching JSON
  - main.js -- not necessarily needed for the UI
  - styles.js

CSS Files:
  - data.css -- contains the styles for the search, filters, and results
  - font-awesome.css -- for the icons used in filters
  - font-awesome.min.css -- minimized version of font-awesome.css
  - main.css

HTML Files:
  - index.html -- .right-panel div children referenced by data.js
  - datamap.json -- referenced by data.js


The primary piece that drives the search functionality for this UI is the 
JSON file (datamap.json). It contains a (currently static) structured list of 
potential datasets and a key mapping of how one might be able to search on said 
datasets. Here is the bare-bones version of the JSON object:

    {
        "title": "",
        "url": "",
        "search": {
            "transect": [
            ],
            "vessel": "",
            "researcher": [
            ],
            "year": [
            ],
            "month": [
            ],
            "keywords": [
            ]
        }
    }

    title - name of the dataset (dataset.title)
    url - Codiac URL to dataset (http://data.eol.ucar.edu/codiac/dss/id=<dataset.archive_ident>)
    search - 
        researcher - name(s) of the dataset authors/originators
        year - year(s) of the dataset collection period
        month - month(s) of the dataset collection period
        keywords - any word or group of words that could help narrow down the 
                   search; can be collected from category / platform  / product /
                   classification as needed
        transect - DBO project specific to transect-lines of vessel runs
        vessel - DBO project specific to vessels used during data collection


To extend this library, simply modify the JSON structure (i.e. search) to 
accommodate which search terms/filters would be useful for your UI. Then modify 
the data.js file to reflect these changes to the JSON structure. Focus, 
specifically, on the filterObjects() and buildResultsTable() functions for the 
JSON structure references.


About filtering:
  In the UI, the more checkboxes that are selected, the more results will show. 
  That is to say, the results are filtered with an OR boolean logic, not an AND 
  boolean logic. The default setting displays all available dataset objects, 
  and displays the subset of dataset objects that match the OR criteria 
  selected.