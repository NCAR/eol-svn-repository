function setHide( dropbox, window, urls )
{
	var index = dropbox.selectedIndex;
	if( dropbox.options[index].value != 'none' )
		window.location = urls[index];
	else
		dropbox.selectedIndex = 0;
}

function editWindow( url )
{
	window.open( url, "McDedit", "width=680,height=730,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function nextId( form, storm_id )
{
	form.storm_id.value = storm_id
}

function setFrames( win, top, left, body )
{
  if( left != null && left != "" )
  {
    win.left.location = left;
  }

  if( top != null && top != "" )
  {
    win.top_frame.location = top;
  }

  if( body != null && body != "" )
  {
    win.body.location = body;
  }

	win.focus();
}

function cursorOver( row )
{
	if( row.style.backgroundColor != '#ffff66' && row.style.backgroundColor != "rgb(255,255,102)" )
		row.style.backgroundColor = '#ebebf5';
}

function cursorOut( row )
{
	if( row.style.backgroundColor != '#ffff66' && row.style.backgroundColor != "rgb(255,255,102)" )
		row.style.backgroundColor = '';
}

function selectRow( box )
{
	var row = "row_" + box.value
	var doit = "document.getElementById( \'" + row + "\' ).style.backgroundColor" 

	if( box.checked )
		eval( doit + "=\'#ffff66\'" )
	else
		eval( doit + "=\'\'" )

}

function checkEmail( form )
{
	var index = form.loader.selectedIndex;	
	var new_loader = form.loader.options[index].value;

	if( new_loader != old_loader && new_loader != 'unassigned' )
	{
		var result = confirm( "Click OK to send an e-mail to the selected loader." );

		if( result )
			form.send_mail.value = "y";
		else
			alert( "No e-mail will be sent for this dataset" ) 
	}
}
