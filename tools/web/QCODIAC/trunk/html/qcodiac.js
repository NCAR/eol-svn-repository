
function submitDs( form, win )
{
	var dataset_id = form.dataset_id.value;

	var url = "supervisor?dataset_id=" + dataset_id;

	if( dataset_id === "" )
	{
		alert( "Please enter a Dataset Id." );
		return;
	}
	var show = 0;

	for( x = 0; x < form.results.length; x++ )
	{
		if( form.results[x].checked )
		{
			url = url + "&results=" + form.results[x].value;
			show = 1;
		}
	}

    url += "&fields=archive_ident&fields=delivery_date&fields=email&isJoss=selected";

	if( show === 0 )
	{
		alert( "Please select which tool(s) to display." );
		return;
	}
	win.display.location = url;
}

function submitProj( form, win )
{
	var proj = form.project.options[form.project.selectedIndex].value;
	// form.project.selectedIndex = 0;

	if( proj == "select" )
		return;

	proj = escape(proj);
	var url = "supervisor?project=" + proj;
	var show = 0;

	for( x = 0; x < form.results.length; x++ )
	{
		if( form.results[x].checked )
		{
			console.log(form.results[x].value);
			url = url + "&results=" + form.results[x].value;
			if(form.results[x].value == "order_statistics") {
				console.log("Success");
				url += "&orders=on&unique_users=on&total_size=on"
				console.log(url);
			}
			show = 1;
		}
	}

	if( show === 0 )
	{
		alert( "Please select which tool(s) to display." );
		form.project.selectedIndex = 0;
		return;
	}
	win.display.location = url;
}

function checkSelect( select, msg )
{
	for( x = 0; x < select.length; x++ )
		if( select[x].selected )
			return true;

	alert( msg );

	return false;
}

function checkText( txt, msg )
{
	if( txt.value !== "" )
		return true;

	alert( msg );

	return false;
}

function checkMultipleText( txt, msg )
{
  for( x = 0; x < txt.length; x++ )
	{
		if( txt[x].value !== "" )
		{
			return true;
		}
	}

	alert( msg );
	return false;
}

// Form validation for the metric form
function checkMetricForm() {
	// Fetch the select element from the DOM
	var select = $('select').children(); 
	// Fetch the checkboxes from the DOM -> Array
	var checkboxes = $(':checkbox');
	// Initialize an empty error string
	var errorString = "";
	// Boolean variable to hold whether an error has been thrown yet or not
	var error = false;

	// Check the select element for validity
	counter = 0;
	for(x = 0; x < select.length; x++) {
		if(select[x].selected) {
			counter++;
		}
	}
	// Check if more than five projects were selected
	if(counter > 5) {
		errorString += "You are not permitted to select more than five projects.";
		error = true;
	}

	// If none were selected, flag an error and add it to the error string
	if(counter === 0) {
		errorString += "You must select at least one project.";
		error = true;
	}

	counter = 0;
	// Check the checkboxes
	for(j = 0; j < checkboxes.length; j++) {
		if(checkboxes[j].checked === true) {
			counter++;
		}
	}

	// If no checkboxes were checked, flag an error and add it to the error string
	if(counter === 0) {
		errorString += "You must select at least one view option.";
		error = true;
	}

	// Format the error string nicely to display in an alert
	if(error === true) {
		errorString = errorString.replace(/([^.!?\s][^.!?]*.)/, "$1 ");
		alert(errorString);
		return false;
	}

	// If no errors were flagged, return true
	return true;
}
