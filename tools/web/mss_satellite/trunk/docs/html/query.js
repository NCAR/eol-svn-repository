
function setCheckBox( box_changed, box_toset, def )
{
	var chg_index = box_changed.selectedIndex;
	var set_index =  box_toset.selectedIndex;

	if( box_toset.options[set_index].text == def )
	{
		box_toset.options[ chg_index ].selected = true;
	}
}

function setTextBox( txt_changed, txt_toset, def )
{
	var chg_val = txt_changed.value
	var set_val = txt_toset.value

	if( set_val == def )
	{
		txt_toset.value = chg_val
	}
}

function setHourChanged( hour, min, def )
{
	var hr_val = hour.value
	var mn_val = min.value

	textChange( hour )
	if( mn_val == def )
	{
		min.value = "00"
	}
}

function textChange( text )
{
	var val = text.value

	if( val.length == 0 )
	{
		val = "00"
	}
	if( val.length == 1 )
	{
		val = "0" + val
	}
	text.value = val
}

function validateForm( form )
{
	var valid = true 
	valid = checkDateTime( form.year_frm, form.month_frm, form.day_frm, form.jdate_frm_yr, form.jdate_frm_dy, form.hour_frm, form.minute_frm, "Begin" );

	if( valid )
		valid = checkDateTime( form.year_to, form.month_to, form.day_to, form.jdate_to_yr, form.jdate_to_dy, form.hour_to, form.minute_to, "End" );

	return valid
}

function checkDateTime( year, month, day, jdate_yr, jdate_dy, hour, minute, type )
{
	var valid = true
	var jyr_i = jdate_yr.selectedIndex;
	var jdy_i = jdate_dy.selectedIndex;

	if( jdate_yr.options[ jyr_i ].text == 'YYYY' && jdate_dy.options[ jdy_i ].text == 'JJJ' )
	{
		var yr = year.value
		var mn = month.value
		var dy = day.value

		if( yr == 'YYYY' || mn == 'MM' || dy == 'DD' )
		{
			valid = false;
			message = "Please Enter a Date"
			year.focus()	
		} 
		else if( yr.length != 4 )
		{
			valid = false
			message = "Year entered is not valid.  Must be in YYYY format"
			year.focus()
		}
		else if( mn.length != 2 || mn < 1 || mn > 12 )
		{
			valid = false
			message = "Month entered is not valid.  Must be in MM format and between 1 and 12."
			month.focus()
		}
		else if( dy.length != 2 || dy < 1 || dy > 31 )
		{
			valid = false
			message = "Day entered is not valid.  Must be in DD format and between 1 and 31."
		}
	} 
	else if( jdate_yr.options[ jyr_i ].text != 'YYYY' && jdate_dy.options[ jdy_i ].text == 'JJJ' )
	{
		valid = false
		message = "Please Select a Julian Day" 
		jdate_dy.focus()
	} 
	else if( jdate_yr.options[ jyr_i ].text == 'YYYY' && jdate_dy.options[ jdy_i ].text != 'JJJ' )
	{
		valid = false
		message = "Please Select a Year"
		jdate_yr.focus()
	}

	if( valid )
	{
		if( hour.value > 23 || hour.value < 0 )
		{
			message = "Hour entered is not valid."
			valid = false
			hour.focus()
		}		
		else if( minute.value > 59 || minute.value < 0 )
		{
			message = "Minute entered is not valid."
			minute.focus()
			valid = false
		}
	}

	if( !valid )
	{
		message = type + " date and time not entered correctly.\n" + message
		alert( message )
	}

	return valid
}

function setSectorList( form )
{
	var sat = getSelected( form.satellite );
	if( sat == "G08" )
	{
		setSectorGoes08( form );	
	}
	else if( sat == "G10" )
	{
		setSectorGoes10( form );
	}
	else if( sat == "G12" )
	{
		setSectorGoes12( form );
	}
	else
	{
		setSectorAll( form )
	}	
}

function getSelected( select )
{
  var list = ""
  var val = "";
  
  for( x = 0; x < select.options.length; x++ )
  {
    list = list + "*" + select.options[x].value
    if( select.options[x].selected )
    {
      val = select.options[x].value
    }
  }

  return val
}

function checkSelection( form )
{
	form.sector.options[form.sector.options.length - 1].selected = false;
}

function resetForm()
{
	queryform.satellite.options[0].selected = true;

	//setSectorGoes12( document.queryform );
	setSectorAll( document.queryform );
}

function setSectorGoes08( form )
{
		form.sector.options[0] = new Option( "ALL", "all", true, true );
		form.sector.options[0].selected = true;
		form.sector.options[1] = new Option( "NOHEM", "1", false, false );
		form.sector.options[1].selected = false;
		form.sector.options[2] = new Option( "NOHEM-EXT", "2", false, false );
		form.sector.options[2].selected = false;
		form.sector.options[3] = new Option( "SOHEM", "3", false, false );
		form.sector.options[3].selected = false;
		form.sector.options[4] = new Option( "SOHEM-LIM", "4", false, false );
		form.sector.options[4].selected = false;
		form.sector.options[5] = new Option( "SOHEM-SS", "5", false, false );
		form.sector.options[5].selected = false;
		form.sector.options[6] = new Option( "CONUS", "6", false, false );
		form.sector.options[6].selected = false;
		form.sector.options[7] = new Option( "SRSO", "7", false, false );
		form.sector.options[7].selected = false;
		form.sector.options[8] = new Option( "FULL", "8", false, false );
		form.sector.options[8].selected = false;
		form.sector.options[9] = new Option( "=================", "null", false, false );
		form.sector.options[9].selected = false;
		form.sector.options.length=10;
		form.sector.size=7
}

function setSectorGoes10( form )
{
		form.sector.options[0] = new Option( "ALL", "all", true, true );
		form.sector.options[0].selected = true;
		form.sector.options[1] = new Option( "NOHEM", "9", false, false );
		form.sector.options[1].selected = false;
		form.sector.options[2] = new Option( "SOHEM", "10", false, false );
		form.sector.options[2].selected = false;
		form.sector.options[3] = new Option( "SOHEM-SS", "11", false, false );
		form.sector.options[3].selected = false;
		form.sector.options[4] = new Option( "PACUS", "12", false, false );
		form.sector.options[4].selected = false;
		form.sector.options[5] = new Option( "SUB-CONUS", "13", false, false );
		form.sector.options[5].selected = false;
		form.sector.options[6] = new Option( "SRSO", "14", false, false );
		form.sector.options[6].selected = false;
		form.sector.options[7] = new Option( "FULL", "15", false, false );
		form.sector.options[7].selected = false;
		form.sector.options[8] = new Option( "=================", "null", false, false );
		form.sector.options[8].selected = false;
		form.sector.options.length=9;
		form.sector.size=7;
}

function setSectorGoes12( form )
{
		form.sector.options[0] = new Option( "ALL", "all", true, true );
		form.sector.options[0].selected = true;
		form.sector.options[1] = new Option( "NOHEM", "16", false, false );
		form.sector.options[1].selected = false;
		form.sector.options[2] = new Option( "NOHEM-EXT", "17", false, false );
		form.sector.options[2].selected = false;
		form.sector.options[3] = new Option( "SOHEM", "18", false, false );
		form.sector.options[3].selected = false;
		form.sector.options[4] = new Option( "SOHEM-LIM", "19", false, false );
		form.sector.options[4].selected = false;
		form.sector.options[5] = new Option( "SOHEM-SS", "20", false, false );
		form.sector.options[5].selected = false;
		form.sector.options[6] = new Option( "CONUS", "21", false, false );
		form.sector.options[6].selected = false;
		form.sector.options[7] = new Option( "SRSO", "22", false, false );
		form.sector.options[7].selected = false;
		form.sector.options[8] = new Option( "FULL", "23", false, false );
		form.sector.options[8].selected = false;
		form.sector.options[9] = new Option( "=================", "null", false, false );
		form.sector.options[9].selected = false;
		form.sector.options.length=10;
		form.sector.size=7
}

function setSectorAll( form )
{
		form.sector.options[0] = new Option( "ALL", "all", true, true );
		form.sector.options[0].selected = true;
		form.sector.options[1] = new Option( "NOHEM (G08)", "1", false, false );
		form.sector.options[1].selected = false;
		form.sector.options[2] = new Option( "NOHEM-EXT (G08)", "2", false, false );
		form.sector.options[2].selected = false;
		form.sector.options[3] = new Option( "SOHEM (G08)", "3", false, false );
		form.sector.options[3].selected = false;
		form.sector.options[4] = new Option( "SOHEM-LIM (G08)", "4", false, false );
		form.sector.options[4].selected = false;
		form.sector.options[5] = new Option( "SOHEM-SS (G08)", "5", false, false );
		form.sector.options[5].selected = false;
		form.sector.options[6] = new Option( "CONUS (G08)", "6", false, false );
		form.sector.options[6].selected = false;
		form.sector.options[7] = new Option( "SRSO (G08)", "7", false, false );
		form.sector.options[7].selected = false;
		form.sector.options[8] = new Option( "FULL (G08)", "8", false, false );
		form.sector.options[8].selected = false;
		form.sector.options[9] = new Option( "NOHEM (G10)", "9", false, false );
		form.sector.options[9].selected = false;
		form.sector.options[10] = new Option( "SOHEM (G10)", "10", false, false );
		form.sector.options[10].selected = false;
		form.sector.options[11] = new Option( "SOHEM-SS (G10)", "11", false, false );
		form.sector.options[11].selected = false;
		form.sector.options[12] = new Option( "PACUS (G10)", "12", false, false );
		form.sector.options[12].selected = false;
		form.sector.options[13] = new Option( "SUB-CONUS (G10)", "13", false, false );
		form.sector.options[13].selected = false;
		form.sector.options[14] = new Option( "SRSO (G10)", "14", false, false );
		form.sector.options[14].selected = false;
		form.sector.options[15] = new Option( "FULL (G10)", "15", false, false );
		form.sector.options[15].selected = false;
		form.sector.options[16] = new Option( "NOHEM (G12)", "16", false, false );
		form.sector.options[16].selected = false;
		form.sector.options[17] = new Option( "NOHEM-EXT (G12)", "17", false, false );
		form.sector.options[17].selected = false;
		form.sector.options[18] = new Option( "SOHEM (G12)", "18", false, false );
		form.sector.options[18].selected = false;
		form.sector.options[19] = new Option( "SOHEM-LIM (G12)", "19", false, false );
		form.sector.options[19].selected = false;
		form.sector.options[20] = new Option( "SOHEM-SS (G12)", "20", false, false );
		form.sector.options[20].selected = false;
		form.sector.options[21] = new Option( "CONUS (G12)", "21", false, false );
		form.sector.options[21].selected = false;
		form.sector.options[22] = new Option( "SRSO (G12)", "22", false, false );
		form.sector.options[22].selected = false;
		form.sector.options[23] = new Option( "FULL (G12)", "23", false, false );
		form.sector.options[23].selected = false;
		form.sector.options[24] = new Option( "=================", "null", false, false );
		form.sector.options[24].selected = false;
		form.sector.size=7
}
