/**
 * Open a new window to display a form to enter/edit travel information.
 * It creates a new window that is 400x350.
 * @param url The url of the form to be opened.
 **/
function openWin(url) {
  myWin= window.open(url, "travel_form", "width=400,height=350,resize=yes");
  myWin.focus();
}

/**
 * Check a specified date to make sure it is in the correct format and that
 * the date actually exists.
 * @param dateInput The date to be checked.
 * @return Return true if it is a valid date in the correct format, false
 * otherwise.
 **/
function checkDate(dateInput){
        var date_format = /[0-9]{4}\/[0-9]{2}\/[0-9]{2}/;
	if (!date_format.exec(dateInput.value)) {
		alert("Date must be in YYYY/MM/DD format.");
		return false;
 	}

	/* Parse the date into its int values */
	var year = dateInput.value.substr(0, 4);
	var month = dateInput.value.substr(5, 2);
	var day = dateInput.value.substr(8, 2);

	/* check the month */
	if(month < 1 || month > 12) {
		alert('You have entered an incorrect month: ' + month);
		return false;
	}

	/* Ensure that the given day actually is in the month */
	if(day < 1 || day > daysInMonth(year, month)){
		alert('You have entered an incorrect date: ' + month + '/' + 
		      day);
		return false;
	}
	return true;
}

/**
 * Get the number of days in a month for the specified year.
 * @param year The year the month occurs in.
 * @param month The month to find the number of days.
 * @return Return the number of days in the specified month and year.
 **/
function daysInMonth(year, month) {
	if (month == 2) {
		if ((year % 4 == 0 && $year % 100 != 0) ||
		    (year % 400 == 0)) {
			return 29;
                } else {
			return 28;
		}
        } else {
		var days = new Array(31,28,31,30,31,30,31,31,30,31,30,31);
		return days[month - 1];
        }
}

/**
 * Check to see that none of the fields in a form are empty.
 * @param form The form containing the fields.
 * @return Returns true if there is a value for all of the fields, false
 * otherwise.
 **/
function checkAllFields(form){
	for(var i = 0; i < form.elements.length; i++){
		if(form.elements[i].value == ""){
			alert('Please fill out all fields.');
			return false;
		}
	}
	return true;
}

/**
 * Check to see if the ID field in a form is unique to all the other known
 * ID values and that the ID does not exceed 8 characters in length.
 * @param form The form containing the ID field.
 * @return Return true if the ID is unique and less than 9 characters, false
 * otherwise
 **/
function checkUniqueID(form) {
        /* Check the length of the ID */
	if (form.id.value.length > 8) { 
	 	alert("Id must be less than 8 characters.");
		return false;	 
        }

	/* Check to see if the id patches the archive tag */
	if (form.id.value.substr(0, 4) == "arch") {
		alert("Id cannot start with arch because of the archive tool.");
		return false;
	}

        /* Check to see if there is a space in the ID */
	if (form.id.value.indexOf(" ") >= 0) {
		alert("ID cannot contain a space.");
		return false;
        }
	
	/* See if there are any IDs that have been used */
	if (form.used_id == null) { return true; }

	var id = form.id.value;
	var idArrayLength = form.used_id.length; /* Get the known ids */

	/* Special case for a single known ID */
	if (idArrayLength == null && form.used_id.value == id) {
		alert('You have chosen an ID that is not unique: ' + 
	              id + '.  Please try again.');
		return false;
        } else {
		for(var i = 0; i < idArrayLength; i++){
			var test_id = form.used_id[i].value;
			if(test_id == id) {
				alert('You have chosen an ID that is not ' +
				      ' unique: ' + id + '.' + 
                                      'Please try again.');
				return false;
			}
		}
		return true;
	}
}

/**
 * Check the employee for to see if all the provided data is valid.
 * @param form The form containing the data.
 * @return Returns true if all of the values are valid, false otherwise.
 **/
function processData(form){
	if(checkAllFields(form)){
		return checkUniqueID(form);
	}
	return false;
}

/**
 * Check the travel form to see if all of the data is valid.
 * @param form The form that contains the data.
 * @return Returns true if all of the provided data is valid, false 
 * otherwise.
 **/
function checkTravelForm(form) {
	if (form.reason.value == "") { form.reason.value = "Unspecified"; }
        if (form.where.value == "") { form.where.value = "Unspecified"; }
	if (!checkDate(form.s_date)) { return false; }
	if (!checkDate(form.e_date)) { return false; }
        if (!checkDateOrder(form.s_date, form.e_date)) { return false; }
	return true;
}

/**
 * Check to see if the first date is before the second date.
 * @param d1 The first date to check.
 * @param d2 The second date to check.
 * @return Returns true if d1 is before or the same as d2, false otherwise.
 **/
function checkDateOrder(d1, d2) {
	var dateParts=d1.value.split("/");
	var start=parseInt(dateParts[0]+dateParts[1]+dateParts[2]);
        dateParts=d2.value.split("/");
	var end=parseInt(dateParts[0]+dateParts[1]+dateParts[2]);
	if (start <= end) { return true; } else { 
		alert("Start date is after the End date.");
		return false; 
	}
}

/**
 * Confirm the deletion of a travel instance for an employee.
 * @param form The form containing the travel information.
 * @return Returns true if the data is to be deleted, false otherwise.
 **/
function deleteTravel(form) {
	return confirm("Delete this travel information for " + form.id.value);
}

/**
 * Check to see that a year is a 4 digit integer.
 * @param form The form containing the data.
 * @return Returns true if the year is a 4 digit integer, false otherwise.
 **/
function checkYear(form) {
	var regex = /[0-9]+/;
	if (!regex.exec(form.year.value) || form.year.value.length != 4) {
		alert("Year must be 4 digits long");
		return false;
	}
	return true;
}













