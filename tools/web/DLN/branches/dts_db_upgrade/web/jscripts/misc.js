function editWindow( url )
{
	window.open( url, "edit", "width=675,height=770,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function showStatus( win, msg )
{
	win.location="../top/status.jsp?message=" + msg
}

function refresh( win, id )
{
	win.location="../body/refresh_view.jsp?id=" + id
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

function setHide( sel, value, win )
{
	var index = sel.selectedIndex;

	var field = sel.options[index].value

	if( field != "prompt" )
		win.main.location = "set_hide.jsp?" + field + "=" + value
}

function setParam( sel, win, form )
{
	var index = sel.selectedIndex;
	
	var param = sel.options[index].value;

	if( param != "prompt" )
	{
		form.param.value = param;
		form.submit();	
	}
	else
		sel.options[0].selected = true;
}

function verify_add(form) {
	var doAdd = confirm( "Are you sure you want to add a new dataset, as opposed to updating this dataset?" );
	if (doAdd) {
		var value = prompt("New Dataset Id",form.datasetId.value);
		if (value != null && value != '') {
			form.datasetId.value = value;
		} else {
			doAdd = false;
		}
	}

	return doAdd;
}

function verify_delete()
{
	return confirm( "This will delete the dataset from the Data Tracking System and in CODIAC." )
}

function checkSendMail(form)
{
	var lindex = form.loaderId.selectedIndex;
	var cindex = form.checkerId.selectedIndex;

	if (form.loaderId.options[lindex].value != 0) {
		var query = "Clicking OK will send an e-mail to " +
						form.loaderId.options[lindex].text + " from " +
						form.checkerId.options[cindex].text + " regarding this new dataset.";

		var ans = confirm( query );
	
		if( ans )
			form.send_mail.value = "y";
		else		
			alert( "No email will be sent for this dataset" );
	}
	return true;	
}

function selectRow( rowNum, box )
{
	var row = "row_" + rowNum
	var doit = "document.getElementById( \'" + row + "\' ).style.backgroundColor"

	if( box.checked )
		eval( doit + "=\'#ffff66\'" )
	else
		eval( doit + "=\'\'" )

}

function checkUsers( form ) {
	if( form.internalContactId.options[form.internalContactId.selectedIndex].value == 0 ) {
		alert( "You must select an Internal Contact for the dataset." );
		return false;
	}

	if (form.loaderId.options[form.loaderId.selectedIndex].value != 0 &&
	    form.checkerId.options[form.checkerId.selectedIndex].value == 0) {
		alert("You must have a Checker assigned when the Loader is assigned.");
		return false;
	}	

	return true;
}

