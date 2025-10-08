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
}

function editWindow( url )
{
	window.open( url, "edit", "width=650,height=700,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function editCatWindow( url )
{
	window.open( url, "editCat", "width=600,height=300,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
}

function editProjWindow( url )
{
	window.open( url, "editProj", "width=600,height=480,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
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
		if( navigator.appName == "Netscape" )
		{
			main_win.focus();
			main_win.body.location = url;
			var txt = new Text();
			txt.name = "editButton";
			txt.value = btn_val;
			form.editButton = txt;
			form.submit();
		}
		else
		{
			form.submit();
			main_win.focus();
			main_win.body.location = url;
		}
	}
}

function refresh( url )
{
	top.opener.parent.focus();
	top.opener.parent.body.location = url;
}

function doCancel( form, main_win )
{
	main_win.focus();	
	if( navigator.appName == "Netscape" )
	{
		var txt = new Text();
		txt.name = "editButton";
		txt.value = btn_val;
		form.editButton = txt;
	}
	form.submit();
}
