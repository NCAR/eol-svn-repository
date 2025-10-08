function build_authors(person) {
	var base = 'http://beringsea.eol.ucar.edu/cgi-bin/m_list/m_query?fields=title&project=BEST&project=BSIERP';
	var tail = '&fields=author&fields=project&fields=documentation&fields=comments&order=author&order=title';
	var author = '' + person;
	
	if ( /%.*%/.test(author) == false && author != '' ) {
		author = '%' + author + '%';
	}
	
	if ( author != '' ) {
		author = '&authors=' + author;
	}
	
	var full_url = base + author + tail;
	return full_url;
}

function build_results(theForm) {
	/* -AO- Build an iframe "file" for a given type of query to the Master List
	 *      and use jQuery to place the "file" into the "r_body" div.
	 */
	var prj = check_btn(theForm);
	prj += '&fields=author&fields=project';
	
	var first_field = '';
	if (theForm.project[1].checked == true) {
		// If BSIERP is checked, include BSIERP project numbers as a column.
		first_field = 'fields=bsierp_id&fields=title';
	} else {
		first_field = 'fields=title';
	}
	
	var base = 'http://beringsea.eol.ucar.edu/cgi-bin/m_list/m_query?' + first_field + prj + '&fields=documentation&fields=comments';
	var tail = '&title=&order=author&order=title';
	var url = check_lists(theForm);
	var full_url = base + url + tail;
	
	$('#r_pop').html('<button type="button" class="btn btn-default right btn-pad-top" onclick="open_win(\'' + full_url + '\')">View table in new window</button>');
	//$('#r_pop').css('float','right');

	//$('#r_body').html(full_url);
	console.log((theForm.name) + " (" + ($('form[name='+theForm.name+']').is('form:hidden')) + ")\t\t" + full_url);
	var datastr = '';

	$.get(full_url, function(data) {
		datastr = data;
		datastr = strip_results(datastr);
		$('#r_body').html(build_query(theForm) + datastr);
		
		// Remove the extra libraries/styles
		$("#r_body title, #r_body script, #r_body link").remove();
		$("#r_body div#results font").remove();
		$("#r_body div#results").children("br").remove();
		
		// Add the table-responsive and table classes to the generated table
		$("#r_body div#results").addClass("table-responsive");
		$("#r_body div#results table#sort-table").addClass("table");
		
		// If the browser width is less than 560px, then re-adjust the results table's width
		if ( $(document).width() <= 560 ) {
			var newWidth = $(document).width() - 54;
			$("#r_body div#results").width(newWidth);
		}
		
		// Add the target attribute to all links within the table
		$("#r_body div#results a").each( function() {
			$(this).attr("target","_blank");
			
			var href = $(this).attr("href");
			if ( /codiac_readme\?storm_id=/.test(href) ) {
				if ( /^\s*Documentation\s*$/.test($(this).html()) ) {
					$(this).html("No Documentation Available");
				}
			
				$(this).parent().html( $(this).html() ); // Remove the link altogether
			}
		});
	});


/* *
	var inputs = "'" + full_url + " #results'";
	$('#r_body').load(full_url, function(response, status, xhr) {
		if (status == "error") {
			var msg = "We're sorry, but there was an error processing your request: &nbsp;&nbsp;<strong>";
			$("#r_body").html(msg + xhr.status + " " + xhr.statusText + "</strong> <br /> <h4>Input:</h4>" + inputs);
		}
	});
* */
}

function build_query(theForm) {
	var str = '';
	var slct = '';
	var tmp = '';
	var flg = false;
	
	str += 'The following list contains data sets from ';
	tmp = '<b>no projects</b>';
	for (var i = 0; i < theForm.project.length; i++) {
		if (theForm.project[i].checked == true) {
			if (!flg) { 
				flg = true;
			} else {
				str += ' and ';
			}
			str += '<b>' + theForm.project[i].value + '</b>';
		}
	}
	if (!flg) {
		str += tmp;
	}
	
	flg = false;
	tmp = ' cruise: ';
	for (var i = 0; i < theForm.cruise.length; i++) {
		if (theForm.cruise[i].selected == true) {
			if (!flg) { 
				flg = true;
				str += ' that have been part of the following ';
			} else {
				slct += ' or ';
				tmp = ' cruises: ';
			}
			slct += '<b>' + theForm.cruise[i].value + '</b>';
		}
	}
	if (flg) {
		str += tmp + slct;
	}

	flg = false;
	for (var i = 0; i < theForm.subject.length; i++) {
		if (theForm.subject[i].selected == true) {
			if (!flg) {
				flg = true;
				str += ' that <!--<i>and</i>--> can be classified under ';
			} else {
				str += ' or ';
			}
			str += '<b>' + theForm.subject[i].value + '</b>';
		}
	}

	str += '.';
	
	return str;
}

function check_btn(theForm) {
	/* -AO- Use the form and its input values to set the "checked" option
	 *      for formation of the CGI script URL. 
	 */
	var prj_val = '';

	for (var i = 0; i < theForm.project.length; i++) {
		if (theForm.project[i].checked == true) {
			prj_val += '&project=' + theForm.project[i].value;
//			set_opts(i, 'on', theForm);
		} else {
//			set_opts(i, 'off', theForm);
		}
	}
	
	if (prj_val == '') {
		prj_val = '&project=';
	}
	
	return prj_val;
}

function check_lists(theForm) {
	/* -AO- Use the form and its input values to set the "checked" option
	 *      for formation of the CGI script URL. 
	 */
	var list_val = '';

	for (var i = 0; i < theForm.cruise.length; i++) {
		if (theForm.cruise[i].selected == true)
			list_val += '&platforms=' + theForm.cruise[i].value;
	}

	for (var i = 0; i < theForm.subject.length; i++) {
		if (theForm.subject[i].selected == true)
			list_val += '&data_types=' + theForm.subject[i].value;
	}

	return list_val;
}

function check_opts(theForm) {
	/* -AO- Checks the current status of the project checkboxes, then calls
	 *      set_opts().
	 */
//	var hasProject = false;
//	var isProject = '';
	var statStr = '';

	for (var i = 0; i < theForm.project.length; i++) {
		if (theForm.project[i].checked == true) {
			set_opts(i, 'on', theForm);
//			hasProject = true;
			statStr += '1';
		} else {
			set_opts(i, 'off', theForm);
//			isProject += '.' + theForm.project[i].value;
			statStr += '0';
		}
	}

	// SPECIAL CASE: For the options that both projects share.
	if ( (statStr == '01') || (statStr == '10') || (statStr == '11') ) {
		$('.best.bsierp').removeAttr('disabled');
	} else if (statStr == '00') {
		$('.best.bsierp').attr('disabled', 'disabled');
	}

	// Unselect all the disabled options
	$(':disabled').each(function () {
		$(this).attr('selected', false);
	});
	
	// Add/remove the disabled class for the completely disabled selects.
	toggleDisabledSelect("cruise", theForm);
	toggleDisabledSelect("subject", theForm);
}

function clear_list(listObj) {
	/* -AO- Clears the listObj list of all selections.
	 */
	for (var i = 0; i < listObj.length; i++) {
		listObj[i].selected = false;
	}
}

function open_win(url) {
	/* -AO- Opens url in a new window rather than a new tab in the browser.
	 */
	var new_win = window.open(url, '', 'location=0,scrollbars=1,width=900,height=700');
}

function reset_opt(theForm) {
	/* -AO- Resets the search form.
	 */

	for (var i = 0; i < theForm.project.length; i++) {
		theForm.project[i].checked = false;
	}

	for (var i = 0; i < theForm.cruise.length; i++) {
		theForm.cruise[i].selected = false;
		theForm.cruise[i].disabled = "disabled";
	}

	for (var i = 0; i < theForm.subject.length; i++) {
		theForm.subject[i].selected = false;
		theForm.subject[i].disabled = "disabled";
	}

	// Add/remove the disabled class for the completely disabled selects.
	toggleDisabledSelect("cruise", theForm);
	toggleDisabledSelect("subject", theForm);

	build_results(theForm);
}

function handle_off_opts(index, theForm) {
	var chk_seq = '';

	for (var i = 0; i < theForm.project.length; i++) {
//		if ( !(theForm.project[i].checked) && (theForm.project[i].value != theForm.project[index].value) ) {
			// If not checked AND it is not the project we were trying to turn off... if BSIERP (true) and BSIERP != BSIERP (true)
		if (theForm.project[i].checked) {
			chk_seq += '1'; // If checked
		} else {
			chk_seq += '0'; // If not checked
		}
	}
	
	switch (chk_seq) {
		case '10':
			
		case '01':

		default: // Case '00'
			$(this).attr('disabled', 'disabled');
			this.selected = false;
	}

}

function set_opts(index, opt, theForm) {
	/* -AO- Sets the options in the multiple selection lists to enabled or
	 *      disabled depending on if the project is checked.
	 */
	var prj_class = '.' + theForm.project[index].value.toLowerCase();

	if ( opt == 'off' ) {
		$(prj_class).each(function() {
			if (!(  ($(this).hasClass('best')) && ($(this).hasClass('bsierp'))  )) {
				$(this).attr('disabled', 'disabled'); // If this element does not have both classes, disable it.
				this.selected = false; // If this element is selected and does not have both classes, unselect it.
			} else {
				handle_off_opts(index, theForm);
			}
		});
	} else if ( opt == 'on' ) {
		$(prj_class).removeAttr('disabled');
	}
}

function strip_results(str) {
	/* -AO- Local method that strips stylesheet links, titles, etc. from the 
	 *      results string fetched from the jQuery load.
	 */
	var newStr = '';

	newStr = str.replace(/<font class=[<>=:"\/\.\i\w\s]*<\/font>/, '');
	newStr = newStr.replace('<center>', '');
	newStr = newStr.replace('</center>', '');
	//newStr = newStr.replace('arctic/css/DEFAULT.css', 'arctic/css/BSIERP.css');

	return newStr;
}

function strip_sp(str) {
	/* -AO- Local method that strips spaces from a given string.
	 */
	var fullStr = '';

	fullStr = str.replace(/ /gi,"+");

	return fullStr;
}

function toggleDisabledSelect(sName, theForm) {
	var currentFormSelect = $("form[name=" + theForm.name + "] select[name=" + sName + "]");
	var totalEnabled = $(currentFormSelect).children(":not([disabled])").length;
	
	if (totalEnabled > 0) {
		$(currentFormSelect).removeClass("disabled");
	} else {
		$(currentFormSelect).addClass("disabled");
	}
}



function getDeviceWidth() {
	if (typeof (window.innerWidth) == 'number') {
		//Non-IE
		return window.innerWidth;
	} else if (document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
		//IE 6+ in 'standards compliant mode'
		return document.documentElement.clientWidth;
	} else if (document.body && (document.body.clientWidth || document.body.clientHeight)) {
		//IE 4 compatible
		return document.body.clientWidth;
	}
	return 0;
}


/* XS Device Handling for the "Find Data" forms */
function updateProject(caller, prjKey) {
	$('#ProjectSelect').attr('value', prjKey);
	setProject();
	
	$(caller).parent("li").siblings().removeClass("active");
	$(caller).parent("li").addClass("active");
}