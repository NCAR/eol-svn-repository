README file for NL EASE-Grid Geolocation tar file

  ------------------------------------------------------------------------

The enclosing tar file contains ancillary data for data sets defined in the
NL (Northern Hemisphere, "low", i.e. 25 km resolution) EASE-Grid. The tar
file includes the following:

   * IDL (Interactive Data Language) program easeconv.pro and help file
     easeconv_help.html (for dataset users with IDL): this program contains
     routines to convert between latitude/longitude and row/column
     coordinates.
   * C source code ezlhconv.c and header file ezlhconv.h (for dataset users
     with knowledge of C programming): subroutines in these files can be
     called from the user's main program, to convert between
     latitude/longitude and row/column coordinates.
   * FORTRAN source code ezlhconv.f (for dataset users with knowledge of
     FORTRAN programming): subroutines in these files can be called from the
     user's main program, to convert between latitude/longitude and
     row/column coordinates.
   * For dataset users who only have need of the latitude and longitude of
     the center of any cell in the grid, geolocation data files contain
     flat, binary arrays of 4-byte integers containing the latitude or
     longitude of the respective grid cell, in hundred-thousandth degrees.
     The user should divide the stored integer value by 100,000. to yield
     decimal degrees, with 1 meter precision. The user is expected to know
     which byte-ordering convention is required by their system. Files are
     provided in both unix (a.k.a. "big-endian" or "MSB") and PC (a.k.a
     "little-endian" or "LSB") byte orders. The NL EASE-Grid has dimensions
     721x721 pixels.

     File naming convention for geolocation files is:

     ggpppooo.GZ

        o gg: grid name {NL, NH, SL, SH, ML, MH}
        o ppp: parameter {LAT, LON}
        o ooo: byte-order, {MSB, LSB}
        o .GZ extension indicates the files are compressed using GNU gzip.

     Depending on the coverage area (this varies for different grids),
     scaled latitudes range from -90.00000 to 90.00000, and scaled
     longitudes range from -180.00000 to 180.00000, with missing data (i.e.
     corners of grids with hemispheric coverage) indicated by scaled value
     14316.55765.

   * N200correct.mpp (Northern Hemisphere map projection parameters (.mpp)
     file) and the Nl.gpd (25-km grid parameter definition (.gpd) file):
     contain projection and grid resolution information specifying a 25-km
     grid resolution, for the full hemisphere, Lambert's equal-area
     projection. This information is required for users who want to use the
     generic software packages in NSIDC's Scientific Programmer's Software
     Library.

For detailed answers to your EASE-Grid questions please see All about
EASE-Grid. Another useful publication for users seeking broader
understanding of the gridding procedures is Points, Pixels, Grids, and
Cells. Please direct all other questions about the contents of this
directory to NSIDC's User Services Office.
