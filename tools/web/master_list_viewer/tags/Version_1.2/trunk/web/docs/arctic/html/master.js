function setLink( form )
{
	var val = form.storm_id.value

	if( val.length <= 10 )
		form.url.value = "http://www.joss.ucar.edu/cgi-bin/codiac/dss?" + val
}

function field( select, table, proj_id, url )
{
	var val = select.value

	url = url + "?Action=updateField&" + table + "_id=" + val + "&project=" + proj_id

	parent.main.location = url
}

function showTable( form, url )
{
	var sel = form.project.selectedIndex
	var project = form.project.options[sel].value

	var sel = form.sort_field.selectedIndex
	var sort_field = form.sort_field.options[sel].value

	var sel = form.sort_dir.selectedIndex
	var sort_dir = form.sort_dir.options[sel].value

	var sel = form.dis_count.selectedIndex
	var dis_count = form.dis_count.options[sel].value

//	var sort_field = form.sort_field.value
//	var sort_dir = form.sort_dir.value
//	var dis_count = form.dis_count.value

	url2 = url + "?Action=display" + "&project=" + project + "&sort_field=" + sort_field +
      "&sort_dir=" + sort_dir + "&dis_count=" + dis_count 

	if( project != -1 )
		parent.main.location = url2
}

function editFields( form, url )
{
	var project = form.project.value
	url2 = url + "?Action=editFields&project=" + project

	if( project != -1 )
		parent.main.location = url2
	else
		alert( "Select a Project Name." );
}

function addProject( url )
{
	parent.main.location = url + "?Action=getAddProject";
}

function clearForm( form )
{
	// Clear all of the fields
	form.storm_id.value = "";
	form.title.value = "";
	form.author.value = "";
	form.doc_url.value = "";
	form.data_type_new.value = "New";
	form.discipline_new.value = "New";
	form.platform_new.value = "New";
	form.site_new.value = "New";
	form.spat_res_new.value = "New";
	form.comments.value = "";
	form.method_of_obs.value = "";
	form.phase.value = "";
	form.data_type.options[0].selected = true;
	form.discipline.options[0].selected = true;
	form.platform.options[0].selected = true;
	form.site.options[0].selected = true;
	form.spat_res.options[0].selected = true;

	// Set today's date in the date field
	today = new Date();
	form.date_year.value = today.getYear();
	var month = today.getMonth();
	var day = today.getDate();
	month++;
	var mn = new String( month );	
	var dy = new String( day );	
	if( mn.length == 1 ) mn = "0" + mn;
	if( dy.length == 1 ) dy = "0" + dy;

	form.date_month.value = mn
	form.date_day.value = dy

	setLink( form );
}

function jumpAnchor()
{
	if( document.anchors.length != 0 )
	{
		anchor = getAnchorPosition( "mark_here" ); 
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
