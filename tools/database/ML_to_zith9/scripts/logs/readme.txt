

November 11, 2021

A question was raised about the integration code:
	Did the ML.xx datasets originally in the dmg_merged_ml database 
get updated to the newly created zith9 dataset IDs?

Answer:
	Yes, over 2500 ML.xxx datasets originally in the ML database were recreated in zith9, 
and the old ML dataset_id beginning with "ML." was updated to the new zith archive_ident. 
These now are in the ML database with the zith9 archive_ident as ID.

	The make_dataset.sh script checked the "quality" flag, and the ML.xx datasets that did 
not have the "quality" flag set did not get created in zith9 and remained as ML.xx datasets 
in the ML database. 
  See Section E: of the "A_List_of_Steps_to_Merge_Master_List.rtf" directions. Also see 
the MLxxxx_datasets-changes_pt1.sql and MLxxxx_datasets-changes_pt2.sql files which have 
the SQL statements that updated the ML.xxxx dataset IDs.

Explanation:
	Datasets in the ML database that were not actual datasets, but placeholders or external 
links only did not have the "quality" flag set, so were not created as separate zith9 
datasets during integration. These remain in the ML database as ML.xxx datasets. They 
are perhaps of historical interest, and could be recreated in zith before the ML is totally 
decommissioned. Therefore a simple change was noted in the code of make_dataset.sh to ensure 
quality is set to "final", no matter how sparse the ML.xx dataset, guaranteeing nothing 
would be lost when the final integration of the ML database is run.  	

