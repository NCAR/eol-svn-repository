#****************************************************************************************************
# dataset_helper.py
# 
# This file is used to run other dataset scripts. Those scripts include
#	- acronyms_list.py: Export list of acronyms from summary in database
#	- output_character_numbser.py: Export datasets with summary char < 140
#	- output_summary.py: Export summary for each dataset
#	- acronyms_clenup.py: Acronyms cleanup script
#	- classification.py: Export txt file contains archive_ident, title, classification. Can be imported into Tree.
# 	- classification_dataset.py: Export csv file contains archive_ident, classification, title, platform *******Need to run platform first********
# 	- platform.py: Export txt file contains archive_ident, title, platform. Can be imported into Tree
#	- platform_dataset.py: Export csv file contains archive_ident, platform, title
#	- remove all the output files
# 
# Syntax: python dataset_helper.py <args>
#	or python dataset_helper.py help for more info
# 
# Author: Yuan Sui
# Date: 07/02/2014
#****************************************************************************************************
import sys
import os
from summary.output_acronyms import output_acronyms
from summary.output_character_number import output_character_number
from summary.output_default import output_default
from summary.output_summary import output_summary
from acronyms_cleanup.acronyms_cleanup import acronyms_cleanup
from platform.classification import classification_tree
from platform.classification_dataset import classification_dataset
from platform.platform_dataset import platform_dataset
from platform.platform import platform_tree

def help_guide():
	print "\nRun script: python dataset_helper.py <args>"
	print"Args:"
	print "[acronyms]"
	print "\tExport list of acronyms from summary in Arctic datasets from zith9"
	print "\tOutput: output_acronyms.csv"
	print "\tOutput file contains: Acronyms, archive_ident_id"
	print "[char_number]"
	print "\tEFind all datasets with the count of characters in summary < 140 from zith9"
	print "\tOutput: output_character_number.csv"
	print "\tOutput file contains: projects, archive_ident_id, dataset_title, count_of_char"
	print "[default_value]"
	print "\tFind all datasets with default date, longitude or latitude from zith9"
	print "\tOutput: output_default.csv"
	print "\tOutput file contains: projects, archive_ident_id, dataset_title, columns_with_defalut_value, default_values"
	print "[summary]"
	print "\tExport summary for each dataset in zith9"
	print "\tOutput: output_summary.csv"
	print "\tOutput file contains: projects, archive_ident_id, dataset_title, summary"
	print "[acronyms_clean]"
	print "\tAcronyms cleanup script"
	print "\tOutput: spell_out_list.csv, acronyms_list.csv"
	print "\tSee Instructions_to_use_acronyms_cleanup.pdf for more details"
	print "[classification_tree]"
	print "\tExport txt file contains archive_ident, title, classification. Can be imported into Tree."
	print "\tOutput: classification_tree.txt"
	print "\tOutput file can be opened in Tree directly"
	print "[classification]"
	print "\tExport csv file contains archive_ident, classification, title, platform"
	print "\t*******Need to run platform first********"
	print "\tOutput: output_classification.csv"
	print "\tOutput file contains: archieve_idnet_id, title, classification, platform"
	print "[platform_tree]"
	print "\tExport txt file contains archive_ident, title, platform. Can be imported into Tree"
	print "\tOutput: platform_tree.txt"
	print "\tOutput file can be opened in Tree directly"
	print "[platform]"
	print "\tExport csv file contains archive_ident, platform, title"
	print "\tOutput: output_platform.csv"
	print "\tOutput file contains: archive_ident_id, dataset_title, platform"
	print "[clean]"
	print "\tRemove all the output files under ./outputs"


if len(sys.argv)<2:
	help_guide()
elif str(sys.argv[1])=="acronyms":
	output_acronyms("outputs/output_acronyms.csv")
elif sys.argv[1]=="char_number":
	output_character_number("outputs/output_character_number.csv")
elif sys.argv[1]=="default_value":
	output_default("outputs/output_default.csv")
elif sys.argv[1]=="summary":
	output_summary("outputs/output_summary.csv")
elif sys.argv[1]=="acronyms_clean":
	acronyms_cleanup("outputs/spell_out_list.csv")
elif sys.argv[1]=="classification_tree":
	classification_tree("outputs/classification_tree.txt")
elif sys.argv[1]=="classification":
	classification_dataset("outputs/output_classification.csv", "outputs/output_platform.csv")
elif sys.argv[1]=="platform_tree":
	platform_tree("outputs/platform_tree.txt")
elif sys.argv[1]=="platform":
	platform_dataset("outputs/output_platform.csv")
elif sys.argv[1]=="clean":
	filelist = [ f for f in os.listdir("./outputs")]
	for f in filelist:
		try:
        		os.remove("./outputs/"+f)
        		print "removed %s" % f
		except OSError:
        		pass
else:
	help_guide()
