// The common JavaScript functions for cement

function warnBeforeContactDelete() { 
     return confirm('Are you sure you want to delete this contact?') 
} 

function warnBeforeDatasetDelete() { 
     return confirm('Are you sure you want to delete this dataset?') 
} 

function cursorOver( row )
{
	row.style.backgroundColor = '#dddddd';
}

function cursorOut( row )
{
	row.style.backgroundColor = 'EBF6FF';
}

