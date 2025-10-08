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
	window.open( url, "editML", "width=650,height=700,menubar=no,toolbar=no,location=no,resizable=yes,directories=no,scrollbars=yes,status=0" ).focus(); 
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
	form.year.value = "0000";
	form.month.value = "00";
	form.day.value = "00";
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


// ------ Layer testing -------------------//
function MM_findObj(n, d) { //v4.0
  var p,i,x;  

	if(!d) 
		d=document; 

	if((p=n.indexOf("?"))>0&&parent.frames.length) 
	{
    d=parent.frames[n.substring(p+1)].document; 
		n=n.substring(0,p);
	}

  if(!(x=d[n])&&d.all) 
		x = d.all[n]; 

	for (i=0;!x&&i<d.forms.length;i++) 
		x = d.forms[i][n];

  for(i=0; !x && d.layers && i< d.layers.length; i++) 
		x=MM_findObj(n,d.layers[i].document);

  if(!x && document.getElementById) 
		x=document.getElementById(n); return x;
}

function showLayer() { //v1.2 by PVII
	var g,b,k,f,args=showLayer.arguments;

	var a = parseInt(args[0]);
	if(isNaN(a))a=0;

	if(!document.p7setc) 
	{
		p7c=new Array();
		document.p7setc=true;

		for (var u=0;u<10;u++) 
		{
			p7c[u] = new Array();
		}
	}

	for(k=0; k<p7c[a].length; k++) 
	{
		if((g=MM_findObj(p7c[a][k]))!=null) 
		{
			b=(document.layers)?g:g.style;
			b.visibility="hidden";
		}
	}

	for(k=1; k<args.length; k++) 
	{
		if((g=MM_findObj(args[k])) != null) 
		{
			b=(document.layers)?g:g.style;
			b.visibility="visible";
			f=false;
			for(j=0;j<p7c[a].length;j++) 
			{
				if(args[k]==p7c[a][j]) 
					f=true;
   		}
			if(!f) 
			{
				p7c[a][p7c[a].length++]=args[k];
			}
		}
	}
}
