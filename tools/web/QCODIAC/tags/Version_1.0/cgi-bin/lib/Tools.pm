
# This is the complete listing of all the available tools for
# this library. To add a new tool to the library, include its
# module ('use module...') and then add an instance of that 
# module to the list in the subroutine below.

package Tools;

use lib ".";
use Shell_Tool;

use On_Line_Phys_Listing;
use Off_Line_Phys_Listing;
use Platforms;
use Id_Types;
use Who_Ordered_Dataset;
use Contacts;
use Proj_Stats;
use Ds_Description;
use Proj_Gen_Info;
use Logical_Fmt;
use Physical_Fmt;
use Media_Code;
use Proj_Ds_List;
use Dodsable;

sub toolListing {
	
	# List of tools in QCODIAC
	my @tools = ( 				
								new Ds_Description,
								new Who_Ordered_Dataset,
								new Dodsable,
								new On_Line_Phys_Listing,
								new Off_Line_Phys_Listing,
								new Contacts,
								new Proj_Gen_Info,
								new Proj_Stats,
								new Proj_Ds_List,
								new Platforms,
								new Id_Types,
								new Logical_Fmt,
								new Physical_Fmt,
								new Media_Code
							);

	return @tools;
}

1;


