function confirmSoftwareDelete() {
	return confirm("Are you sure you want to remove the software for this data set?\n"+
				   "This will not affect general software information.");
}

function confirmSourceDelete() {
	return confirm("Are you sure you want to remove the data set as a source for this product?\n"+
				   "This will not affect general source information.");
}

/**
 * Expand/contract a block to display more/less information to the user.
 * @param button The button being pressed to change the state.
 * @param blockId The ID of the block being expanded/contracted.
 * @param contractStyle The style to display when the block is in its contracted state.
 * @param expandStyle The style to display when the block is in its expanded state.
 */
function expand(button, blockId, contractStyle, expandStyle) {
	var divBlock = document.getElementById(blockId);
	
	if (button.value == 'Expand') {
		divBlock.className = expandStyle;
		button.value = "Shrink";
	} else {
		divBlock.className = contractStyle;
		button.value = "Expand";
	}
}

function openNewProcessedDataset(url) { openWindow(url, 800, 300); }

function openNoteWindow(url) { openWindow(url, 800, 600); }

function openProcessStatusWindow(url) { openWindow(url, 800, 650); }

function openSoftwareWindow(url) { openWindow(url, 800, 650); }

function openSourceWindow(url) { openWindow(url, 800, 650); }

/**
 * Expand/contract the set of table rows that display the source information for a data set.
 * @param button The button being pressed to change the display state.
 * @param rowName The name of the table rows to be searched for to change the state.
 * @param contractStyle The style to display when the row is in its contracted/hidden state.
 * @param expandStyle The style to display when the row is in its expanded/visible state.
 */
function sourceExpander(button, rowName, contractStyle, expandStyle) {
	var rows = document.getElementsByName(rowName);
	
	for (var i = 0; i < rows.length; i++) {
		if (button.value == 'Expand') {
			rows[i].className = expandStyle;
		} else {
			rows[i].className = contractStyle;
		}
	}
	
	button.value = button.value == 'Expand' ? "Shrink" : "Expand";
}

function validateProcessStatusForm(form) {
	return validateNoteForm(form);
}

function validateSoftwareForm(form) {

    form.name.value = form.name.value.replace(/^\\s+/i, "");
    form.name.value = form.name.value.replace(/\\s+$/i, "");

	if (form.name.value == '') {
		alert("The software package must have a name.");
		return false;
	}
	
	return true;
}

/**
 * Display a confirmation dialog to the user to ensure they truly want to delete the data set
 * from the database.
 * @return <code>true</code> if the user wants to delete the data set, <code>false</code>
 * if they do not.
 */
function verify_delete() {
    return confirm( "This will delete the processing task of the dataset from IVEN." )
}








function selView( dropbox, urls )
{
	var index = dropbox.selectedIndex;
	if( dropbox.options[index].value != 'dummy' )
		window.location = urls[index-1] 
	else
		dropbox.selectedIndex = 0;
}

function editWindow( url )
{
	window.open( url, "Ivenedit", "width=680,height=730,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function dsAdminEditWin( url )
{
	window.open( url, "dsAdminEdit", "width=680,height=680,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

/*
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
*/
function selectRow( box )
{
	var row = "row_" + box.value
	var doit = "document.getElementById( \'" + row + "\' ).style.backgroundColor" 

	if( box.checked )
		eval( doit + "=\'#ffff66\'" )
	else
		eval( doit + "=\'\'" )

}

function expandText( div_id, size, img_src )
{
	var img_id = div_id + "_img";
	var a_id = div_id + "_a";

	for( var x = 0; x < size; x++ )
	{
		var div_id2 = div_id + "_" + x;
		document.getElementById( div_id2 ).style.overflow = "visible";
	}

	var old_img_src = document.getElementById( img_id ).src;
	document.getElementById( img_id ).src = img_src; 
	document.getElementById( a_id ).href = "javascript: shrinkText( \'" + div_id + "\'," + size + ", \'" + old_img_src + "\');";
}


function shrinkText( div_id, size, img_src )
{
	var img_id = div_id + "_img";
	var a_id = div_id + "_a";

	for( var x = 0; x < size; x++ )
	{
		var div_id2 = div_id + "_" + x;
		document.getElementById( div_id2 ).style.overflow = "auto";
	}

	var old_img_src = document.getElementById( img_id ).src;
	document.getElementById( img_id ).src = img_src; 
	document.getElementById( a_id ).href = "javascript: expandText( \'" + div_id + "\'," + size + ", \'" + old_img_src + "\');";
}

//--------- Stuff for the auto-scroll, not my code---------------//

function jumpAnchor()
{
	var our_anchor = "pdanchor";

	if((document.anchors.length != 0) && (document.all[our_anchor]))
	{
		anchor = getAnchorPosition( our_anchor ); 
		window.scroll( anchor.x, anchor.y )
	}
}



//--------------------anchor.js----------------------------------//
// getAnchorPosition(anchorname)
//   This function returns an object having .x and .y properties which are the coordinates
//   of the named anchor, relative to the page.
function getAnchorPosition(anchorname) {
	// This function will return an Object with x and y properties
	var useWindow=false;
	var coordinates=new Object();
	var x=0,y=0;
	// Browser capability sniffing
	var use_gebi=false, use_css=false, use_layers=false;
	if (document.getElementById) { use_gebi=true; }
	else if (document.all) { use_css=true; }
	else if (document.layers) { use_layers=true; }
	// Logic to find position
 	if (use_gebi && document.all) {
		x=AnchorPosition_getPageOffsetLeft(document.all[anchorname]);
		y=AnchorPosition_getPageOffsetTop(document.all[anchorname]);
		}
	else if (use_gebi) {
		var o=document.getElementById(anchorname);
		x=AnchorPosition_getPageOffsetLeft(o);
		y=AnchorPosition_getPageOffsetTop(o);
		}
 	else if (use_css) {
		x=AnchorPosition_getPageOffsetLeft(document.all[anchorname]);
		y=AnchorPosition_getPageOffsetTop(document.all[anchorname]);
		}
	else if (use_layers) {
		var found=0;
		for (var i=0; i<document.anchors.length; i++) {
			if (document.anchors[i].name==anchorname) { found=1; break; }
			}
		if (found==0) {
			coordinates.x=0; coordinates.y=0; return coordinates;
			}
		x=document.anchors[i].x;
		y=document.anchors[i].y;
		}
	else {
		coordinates.x=0; coordinates.y=0; return coordinates;
		}
	coordinates.x=x;
	coordinates.y=y;
	return coordinates;
	}

// Functions for IE to get position of an object
function AnchorPosition_getPageOffsetLeft (el) {
	var ol=el.offsetLeft;
	while ((el=el.offsetParent) != null) { ol += el.offsetLeft; }
	return ol;
	}
function AnchorPosition_getWindowOffsetLeft (el) {
	return AnchorPosition_getPageOffsetLeft(el)-document.body.scrollLeft;
	}	
function AnchorPosition_getPageOffsetTop (el) {
	var ot=el.offsetTop;
	while((el=el.offsetParent) != null) { ot += el.offsetTop; }
	return ot;
	}
function AnchorPosition_getWindowOffsetTop (el) {
	return AnchorPosition_getPageOffsetTop(el)-document.body.scrollTop;
	}


