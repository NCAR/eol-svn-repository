/**
 * Update the data set view to display only the data set notes written by the selected
 * author in the drop down box.
 * @param drop The drop down box containing the author options.
 * @param datasetId The ID of the data set shown in the view.
 * @param entryDate The entry date version of the data set shown in the view.
 */
function changeAuthorFilter(drop, datasetId, entryDate) {
    location.replace("/dts/dln/body/view_dataset.jsp?authorFilterId=" + drop.options[drop.selectedIndex].value +
            "&datasetId=" + datasetId + "&entryDate=" + entryDate);
}

/**
 * Update the data set view to display only the data set notes of the note type selected
 * in the drop down box.
 * @param drop The drop down box containing the note type options.
 * @param datasetId The ID of the data set shown in the view.
 * @param entryDate The entry date version of the data set shown in the view.
 */
function changeNoteTypeFilter(drop, datasetId, entryDate) {
    location.replace("/dts/dln/body/view_dataset.jsp?noteTypeFilterId=" + drop.options[drop.selectedIndex].value +
            "&datasetId=" + datasetId + "&entryDate=" + entryDate);
}

/**
 * Change the visibility of the data sets to be shown in the data set list.  (i.e.  Hide the
 * loaded data sets.)
 * @param sel The drop down box containing the values to be changed.
 * @param visibility if the selected value is visible or not.
 */
function changeVisibility(sel, visibility) {
    // Only change the visibility if the selected value is not a place holder.
    if (sel.options[sel.selectedIndex].value != "prompt") {
        location.replace("/dts/dln/body/dataset_list.jsp?" + sel.options[sel.selectedIndex].value + "=" + visibility);
    }
}

/**
 * Open a new window to edit a data set.
 * @param url The URL of the form including any parameters needed to edit an existing data set.
 */
function editDataset( url ) {
	window.open( url, "edit", "width=1200,height=850,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

/**
 * Open a new window to edit a note.
 * @param url The URL to the form including any parameters needed to edit an exisint note.
 */
function editNote(url) {
    window.open(url, "edit", "width=800,height=500,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0").focus();
}

/**
 * Open a new window to edit a project.
 * @param url The URL to the form including any parameters needed to edit an existing project.
 */
function editProject(url) {
    window.open(url, "edit", "width=700,height=770,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0").focus();
}

/**
 * Open a window to edit a contact/user.
 * @param url The URL to the form including any parameters need to edit an existing contact/user.
 */
function editUser(url) {
    window.open(url, "edit", "width=700,height=770,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0").focus();
}

/**
 * Open the QCODIAC tool in a new window.
 * @param url The URL to open the QCODIAC tool to the correct page.
 */
function openQCODIAC(url) {
    window.open(url, "_blank", "");
}

/**
 * Refresh the current view with the specified data set information.
 * @param win The window where the view is displayed.
 * @param datasetId The ID of the data set to be displayed.
 * @param entryDate The entry date of the version of the data set to be displayed.
 */
function refresh(win, datasetId, entryDate) {
    if (win.location.href.search(/view_dataset/i) >= 0 && datasetId != null) {
       win.location.replace("/dts/dln/body/view_dataset.jsp?datasetId="+datasetId+"&entryDate="+entryDate);
    } else {
       if (datasetId != null) {
           var newLocation = "/dts/dln/body/dataset_list.jsp?datasetId="+datasetId+"&entryDate="+entryDate+"#"+datasetId;
           if (win.location.href.search(/newLocation$/i)) {
               win.location.reload(true);
           } else {
               win.location.replace(newLocation);
           }
       } else {
           win.location.replace("/dts/dln/body/dataset_list.jsp");
       }
    }
}

/**
 * Update the status message in the top frame of the main DLN display.
 * @param win The window where the message is to be displayed.
 * @param msg The message to display in the window.
 */
function showStatus(win, msg) {
    win.location.replace("/dts/dln/top.jsp?message=" + msg);
}

/**
 * Perform checks on the data set entry form to ensure that there are valid values
 * for the necessary fields to properly update the database.
 * @param form The form containing the values to be checked.
 * @return <code>true</code> if the values in the form are valid, <code>false</code>
 * if there is at least one invalid value in the form.
 */
function validateForm(form) {
    // Make sure the data set name is not empty.
    form.name.value = form.name.value == null ? "" : form.name.value;
    form.name.value = form.name.value.replace(/^\s+/, '');
    form.name.value = form.name.value.replace(/\s+$/, '');
   
//alert("("+name+")  "+name.length);
 
    if (form.name.value == '') {
        alert("You can not have an empty name for a data set.");
        return false;
    }

    // Make sure the internal contact is selected for the data set.
    if (form.internalContact.options[form.internalContact.selectedIndex].value <= 0) {
        alert( "You must select an Internal Contact for the dataset." );
        return false;
    }
    
    // Determine the number of projects to be disassociated with the data set.
    // Need to check for a single item list as well as looping through a potential list
    var deletedProjectCount = -1;
    if (form.deletedProjects != null) {
            deletedProjectCount = form.deletedProjects.checked == null || !form.deletedProjects.checked ? 0 : 1;
	    for (var i = 0; i < form.deletedProjects.length; i++) {
	        deletedProjectCount += (form.deletedProjects[i].checked ? 1 : 0);
	    }
	}    
   
    // Make sure there is at least one project association with the data set.
    // Need to subtract one from the project length to exclude the add project drop down box.
    if (deletedProjectCount == -1 || form.project.length - 1 <= 0 || form.project.length - 1 <= deletedProjectCount) {
        alert("You must have at least one project associated with the data set.");
        return false;
    } 
    
    
    // Validate the note section of the form.
    return validateNoteForm(form);
}

function validateProject(form) {
    if (form.projectId.value == '') {
    	alert("The project must have a defined project id.");
    	return false;
    }
    
    var start = form.beginDate.value;
    var end = form.endDate.value;
    
    start.replace(/^\\s+/i, "");
    start.replace(/\\-/i, "");
    start.replace(/\\s+$/i, "");
    
    end.replace(/^\\s+/i, "");
    end.replace(/\\-/i, "");
    end.replace(/\\s+$/i, "");
    
    if (end < start) {
    	alert("The begin date is after the start date.");
    	return false;
    }
    
    
    return true;
}

function validateUser(form) {
	if (form.shortName.value == "" || form.personName.value == '') {
		alert("The User must have a defined short name and person name.");
		return false;
	}
	return true;
}

/**
 * Display a confirmation dialog to the user to ensure they want to add a new data set
 * when they are an edit mode of another data set.
 * @return <code>true</code> if the user wants to add the data set as a new data set,
 * <code>false</code> if the user does not want to add the data set.
 */
function verify_add() {
    return confirm( "Are you sure you want to add a new dataset, as opposed to updating this dataset?" )
}

/**
 * Display a confirmation dialog to the user to ensure they truly want to delete the data set
 * from the database.
 * @return <code>true</code> if the user wants to delete the data set, <code>false</code>
 * if they do not.
 */
function verify_delete() {
    return confirm( "This will delete the dataset from the Data Loading Notes." )
}
