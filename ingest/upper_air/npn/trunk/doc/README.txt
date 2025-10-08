-------------------------------------------------------------------------------
Notes for HPSS_NPN_wind_rass.pl
-------------------------------------------------------------------------------
What needs to be finished:
- add to cron tab (should be run once every 4 days)


-------------------------------------------------------------------------------
CODIAC Datasets Affected:
  100.019  NOAA Profiler Network (NPN) Six Minute Wind Profile Data (NOAA/ESRL)
  100.020  NOAA Profiler Network (NPN) Hourly RASS Data (NOAA/ESRL)
  100.021  NOAA Profiler Network (NPN) Hourly Wind Profile Data (NOAA/ESRL)

Directories Involved:
- Ingest
  - 100.019:  /net/ldm/data/profiler
  - 100.020:  /net/ldm/data/rass
  - 100.021:  /export/ldm/data/profiler/hourly
- Archive (on HPSS)
  - 100.019:  /EOL/operational/upper_air/profiler/npn/winds_6min
  - 100.020:  /EOL/operational/upper_air/profiler/npn/rass
  - 100.021:  /EOL/operational/upper_air/profiler/npn/winds_hourly
- Working Directory:
    /net/work/operational/sounding/NPN/


NOTE: /net/ldm/ and /export/ldm/ are both currently pointing to the same
      location


-------------------------------------------------------------------------------
Here is a breakdown of how it works:
  - The script executes a call to process the ingest data files for each 
    dataset (100.019, 100.020, and 100.021)

  - Each process call (one call per dataset ID) will:

    1) Locate the ingest directory, parse the filenames for an expected match, 
       and sort the matching files by day into TAR files (located in the 
       working directory /net/work/operational/sounding/NPN)
       - The script looks for files that were older than today, but more 
         recent than the end date listed in the dataset
       - The date of today is acquired from the current timestamp in UTC
         format
       a) If files are found, the script will then:
          - TAR those files together by Julian day (e.g. 2012111_npn.tar for
            20 April 2012)
          - TAR files have a size of approximately 30 MB – 40 MB per file for
            100.019, approximately 180 KB - 190 KB per file for 100.020, and
            approximately 5 MB - 7 MB per file for 100.021
       b) Each TAR file is checked for its total number of files versus the 
          expected number of files.  If these numbers do not match (i.e. the 
          total number of files within the TAR file is less than the expected
          number of files), then the script will:
          - generate an inventory file for that TAR file in the 
            $work_dir/tarInventories directory
          - Send an e-mail to Scot Loehrer and Linda Cully informing them of
            the new inventory file

    2) Place the daily TAR files onto HPSS in the directory associated with 
       that dataset
         - If the file already exists on the HPSS, this will be noted and sent
           out in an e-mail to Scot Loehrer and Linda Cully once the process 
           call for that dataset is complete
         - If the file already exists on the HPSS, it is assumed that it 
           already exists as a data file associated with that dataset in the 
           database

    3) If the HPSS transfer for that file is successful, it will insert that 
       file into the database
       - If the file is newer than the end date of the dataset, the dataset end
         date will be updated to include the date of that file
       - Any error messages during the insertion process will also be 
         collected and sent out along with any error messages during transfers
         up to HPSS

    4) If any error messages were gathered, an e-mail will be sent out to Scot 
       Loehrer and Linda Cully containing the error message report for that 
       dataset

  - The script will continue to process the ingest data files for any remaining
    datasets, regardless of whether error messages were collected and sent out
    via e-mail

Further details can be found at:
  http://wiki.eol.ucar.edu/cds/DevelopmentDetailsFromScot

This script does not handle the data from 2002-2011 for the hourly RASS 
(100.020).
-------------------------------------------------------------------------------
