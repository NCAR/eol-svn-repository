
function submitDs( form, win )
{
	var dataset_id = form.dataset_id.value

	var url = "supervisor?dataset_id=" + dataset_id; 

	if( dataset_id == "" )
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

	if( show == 0 )
	{
		alert( "Please select which tool(s) to display." );
		return;
	}
	win.display.location = url;
}

function submitProj( form, win )
{
	var proj = form.project.options[form.project.selectedIndex].value;
 	form.project.selectedIndex = 0;

	if( proj == "select" )
 		return;

	proj = escape(proj);
	var url = "supervisor?project=" + proj;
	var show = 0;

	for( x = 0; x < form.results.length; x++ )
	{
		if( form.results[x].checked )
		{
			url = url + "&results=" + form.results[x].value;
			show = 1;
		}
	}

	if( show == 0 )
	{
		alert( "Please select which tool(s) to display." );
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
	if( txt.value != "" )
		return true;

	alert( msg );

	return false;	
}

function checkMultipleText( txt, msg )
{
  for( x = 0; x < txt.length; x++ )
	{
		if( txt[x].value != "" )
		{
			return true;
		} 
	}

	alert( msg );
	return false;
}
