/**
 * Query the user to see if they want to send an email to the appropriate ingest, load, and/or check
 * contacts with the changes made to the data set.
 * @param form The form containing the information to be emailed to the users.
 * @return <code>true</code>
 */
function checkSendMail(form) {
    if (confirm("Do you want to send an email to the appropriate contacts?\n Yes = OK     No = Cancel")) {
       form.send_mail.value = "y";
    }

    return true;    
}

/**
 * Change the hovered row style class.  This works for both the onmouseover
 * and onmouseout commands using the hover style class on in the over and the original
 * style class on the out.
 * @param row The row to have its style class changed.
 * @param styleClass The new style class for the row.
 */
function rowHover(row, styleClass) { row.className = styleClass; }

function openWindow(url, width, height) {
	window.open(url, "edit", "width="+width+",height="+height+",menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

/**
 * Display a confirmation dialog to the user to ensure they want to add a new version to the
 * current data set instead of the assumed update.
 * @return <code>true</code> if the user wants to add the new version of the data set,
 * <code>false</code> if the user does not want to add a new version.
 */
function verify_add_version() {
    return confirm( "Are you sure you want to add a new version of the dataset, as opposed to updating it?" )
}

/**
 * Perform checks on the note entry form to ensure that there are valid values
 * for the fields to properly update the database.
 * @param form The form containing the values to be tested.
 * @return <code>true</code> if the values in the form are valid, <code>false</code>
 * if there is at least on invalid value in the form.
 */
function validateNoteForm(form) {
    // Determine how many note types are selected for the note.
    var checkedCount = 0;
    if (form.noteTypes != null) {
	    for (var i = 0; i < form.noteTypes.length; i++) {
	       if (form.noteTypes[i].checked) { checkedCount++; }
	    }
	}
    
    // Trim leading and trailing white space.
    form.noteText.value = form.noteText.value.replace(/^\\s+/i, "");
    form.noteText.value = form.noteText.value.replace(/\\s+$/i, "");
    
    // Make sure the note is not empty, has an author, and at least one note type.
    if (form.noteText.value != '' && (form.author.value <= 0 || (checkedCount == 0 && form.type.value != 'project'))) {
       if (form.type.value == 'project') {
           alert("The note must have an author.");
       } else {
           alert("The note must have an author and at least one note type.");
       }
       return false;
    } else if (form.noteText.value == '' && (checkedCount != 0 && form.type.value != 'project')) {
       alert ("The note text is empty with a defined note type.");
       return false;
    }
    
    
    
    // The form is valid.
    return true;
}

