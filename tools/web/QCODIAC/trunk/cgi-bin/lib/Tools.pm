#! /usr/bin/perl -w

# Tools.pm
#
# This is the complete listing of all the available tools for
# this library. To add a new tool to the library, include its
# module ('use module...') and then add an instance of that
# module to the list in the subroutine below.

#-----------------------------------------------------------------------------#
#                                Subroutines                                  #
#-----------------------------------------------------------------------------#
# toolList()
#   * Return a listing of all tools to be displayed on the QCodiac webapp
#-----------------------------------------------------------------------------#
#
# Rev. 11/11/2015
# Remove Proj_Stats module (previous commented out), fix whitespace.
# - Eric Dattore

package Tools;

use lib ".";
use Shell_Tool;

use Ds_Description;
use Who_Ordered_Dataset;
use File_Listing;
use Tape_Listing;
use Ds_Xlink_List;

use Proj_Gen_Info;
use Proj_Ds_List;
use Proj_Xlink_List;

use Categories;
use Contacts;
use Formats;
use Frequencies;
use Mediums;
use Platforms;
use Xlinks;
use Order_Statistics;

sub toolListing {
    # List of tools in QCODIAC
    my @tools = (new Ds_Description,
        new Who_Ordered_Dataset,
        new File_Listing,
        new Tape_Listing,
        new Ds_Xlink_List,

        new Proj_Gen_Info,
        new Proj_Ds_List,
        new Proj_Xlink_List,

        new Categories,
        new Contacts,
        new Formats,
        new Frequencies,
        new Mediums,
        new Platforms,
        new Xlinks,
        new Order_Statistics
    );

    return @tools;
}

1;


