function setFrames( win, top, left, body )
{
  if( body != null && body != "" )
  {
    win.body.location = body;
  }

  if( left != null && left != "" )
  {
    win.left.location = left;
  }

  if( top != null && top != "" )
  {
    win.top_frame.location = top;
  }

}

function editWindow( url )
{
	window.open( url, "McDedit", "width=680,height=730,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function editCatWindow( url )
{
	window.open( url, "editCatML", "width=600,height=300,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function editProjWindow( url )
{
	window.open( url, "editProjML", "width=600,height=500,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function today( form )
{
	var today = new Date();
	var year = today.getYear();
	var month = today.getMonth() + 1;
	var day = today.getDate();

	if( year < 2000 )
    year = 2000 + year % 100;

	form.year.value = year;

	if( month < 10 )
		month = "0" + month;
	form.month.value = month;

	if( day < 10 )
		day = "0" + day;
	form.day.value = day;
}

function blank( form )
{
	form.year.value = "";
	form.month.value = "";
	form.day.value = "";
}

function noStormId( form )
{
	form.storm_id.value = "99.999";

}

function setUrl( form, check_url )
{
	var storm_id = form.storm_id.value
	var url = form.url.value

	if( !check_url )
		url = ""
	
	if( storm_id != "99.999" && storm_id != "" && url == "" )
		form.url.value = "http://www.joss.ucar.edu/cgi-bin/codiac/dss?" + storm_id	

	if( !check_url && storm_id == "99.999" )
		form.url.value = "";
}

///*function doSubmit( btn_val, form, main_win, url )
//{
//	var submit = true;
//	url = url + "&sleep=yes"
//	if( btn_val == "Delete" )
//	{
//		submit = confirm( "This will delete the dataset from all categories." );
//	}
//
//	if( !submit )
//	{
//		return false;
//	}
//	else
//	{
//		if( navigator.appName == "Netscape" )
//		{
//			main_win.focus();
//			main_win.body.location = url;
//			var txt = new Text();
//			txt.name = "editButton";
//			txt.value = btn_val;
//			form.editButton = txt;
//			form.submit();
//		}
//		else
//		{
//			form.submit();
//			main_win.focus();
//			main_win.body.location = url;
//		}
//	}

//	return false;
//}

function doSubmit( btn_val, form, main_win, url )
{
	var submit = true;
	url = url + "&sleep=yes"
	if( btn_val == "Delete" )
	{
		submit = confirm( "This will delete the dataset from all categories." );
	}

	if( !submit )
	{
		return false;
	}
	else
	{
		main_win.focus();
		main_win.body.location = url;
	}

	return true;
}

function refresh( url )
{
	//top.opener.parent.focus();
//alert( url );
	top.opener.parent.body.location = url;
}

function doCancel( form, main_win )
{
	main_win.focus();	
	return true;
}

function checkProject( form )
{
	if( form.name.value == '' )
	{
		alert( "Must enter a unique Project Name." );
		return false;
	}
	if( form.left_css_src.value == '' )
	{
		alert( "Must enter a Left CSS Source.  Use 'left_def.css' as a default." );
		return false;
	}
	else if( form.body_css_src.value == '' )
	{
		alert( "Must enter a Body CSS Source.  Use 'body_def.css' as a default." );
		return false;
	}

	return true;
}
