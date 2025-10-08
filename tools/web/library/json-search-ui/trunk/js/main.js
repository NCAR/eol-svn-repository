function build_results() {
  /* -AO- Build an iframe "file" for a given type of query to the Master List
   *      and use jQuery to place the "file" into the "r_body" div.
   */
  var prj = check_btn();
  prj += '&fields=author'; //&fields=project';

  // Add &fields=discipline to base?
  var base = 'http://pacmars.eol.ucar.edu/cgi-bin/m_list/m_query?fields=title' + prj + '&fields=documentation&fields=comments';
  var tail = '&title=&order=author&order=title';
  var url = check_lists();
  var full_url = base + url + tail;

  $('#r_pop').html('<input type="button" value="View table in new window" onclick="open_win(\'' + full_url + '\')" />');
  $('#r_pop').css('float','right');

//$('#r_body').html(full_url);
  var datastr = '';
  $.get(full_url, function(data) {
    datastr = data;
    datastr = strip_results(datastr);
    $('#r_body').html(build_query() + datastr);
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

function build_query() {
  var str = '';
  var slct = '';
  var tmp = '';
  var flg = false;

  str += 'The following list contains data sets from ';
  tmp = '<b>no projects</b>';
  for (var i = 0; i < document.dta_form.project.length; i++) {
    if (document.dta_form.project[i].checked == true) {
      if (!flg) { 
        flg = true;
      } else {
        str += ' and ';
      }
      str += '<b>' + document.dta_form.project[i].value + '</b>';
    }
  }
  if (!flg) {
    str += tmp;
  }

  if (document.dta_form.pi != undefined) {
    flg = false;
    tmp = ' researcher: ';
    for (var i = 0; i < document.dta_form.pi.length; i++) {
      if (document.dta_form.pi[i].selected == true) {
        if (!flg) { 
          flg = true;
          str += ' that have been part of the following ';
        } else {
          slct += ' or ';
          tmp = ' researchers: ';
        }
        slct += '<b>' + document.dta_form.pi[i].innerHTML + '</b>';
      }
    }
    if (flg) {
      str += tmp + slct;
    }
  }

  if (document.dta_form.cruise != undefined) {
    flg = false;
    tmp = ' cruise: ';
    for (var i = 0; i < document.dta_form.cruise.length; i++) {
      if (document.dta_form.cruise[i].selected == true) {
        if (!flg) { 
          flg = true;
          str += ' that have been part of the following ';
        } else {
          slct += ' or ';
          tmp = ' cruises: ';
        }
        slct += '<b>' + document.dta_form.cruise[i].value + '</b>';
      }
    }
    if (flg) {
      str += tmp + slct;
    }
  }

  flg = false;
  for (var i = 0; i < document.dta_form.subject.length; i++) {
    if (document.dta_form.subject[i].selected == true) {
      if (!flg) {
        flg = true;
        str += ' that <!--<i>and</i>--> can be classified under ';
      } else {
        str += ' or ';
      }
      str += '<b>' + document.dta_form.subject[i].value + '</b>';
    }
  }

  str += '.';

  return str;
}

function check_btn() {
  /* -AO- Use the form and its input values to set the "checked" option
   *      for formation of the CGI script URL. 
   */
  var prj_val = '';

  for (var i = 0; i < document.dta_form.project.length; i++) {
    if (document.dta_form.project[i].checked == true) {
      prj_val += '&project=' + document.dta_form.project[i].value;
//      set_opts(i, 'on');
    } else {
//      set_opts(i, 'off');
    }
  }

  if (prj_val == '') {
    prj_val = '&project=';
  }

  return prj_val;
}

function check_lists() {
  /* -AO- Use the form and its input values to set the "checked" option
   *      for formation of the CGI script URL. 
   */
  var list_val = '';

  if (document.dta_form.pi != undefined) {
    for (var i = 0; i < document.dta_form.pi.length; i++) {
      if (document.dta_form.pi[i].selected == true) {
        list_val += '&authors=%' + document.dta_form.pi[i].value + '%';
        
        var lfAuthor = lastname_firstname(document.dta_form.pi[i].value);
        
        if (lfAuthor != '')
			list_val += '&authors=%' + lfAuthor + '%';
      }
    }
  }

  if (document.dta_form.cruise != undefined) {
    for (var i = 0; i < document.dta_form.cruise.length; i++) {
      if (document.dta_form.cruise[i].selected == true)
        list_val += '&platforms=' + document.dta_form.cruise[i].value;
    }
  }

  for (var i = 0; i < document.dta_form.subject.length; i++) {
    if (document.dta_form.subject[i].selected == true)
      list_val += '&data_types=' + document.dta_form.subject[i].value;
  }

  return list_val;
}

function check_opts() {
  /* -AO- Checks the current status of the project checkboxes, then calls
   *      set_opts().
   */
//  var hasProject = false;
//  var isProject = '';
  var statStr = '';

  for (var i = 0; i < document.dta_form.project.length; i++) {
    if (document.dta_form.project[i].checked == true) {
      set_opts(i, 'on');
//      hasProject = true;
      statStr += '1';
    } else {
      set_opts(i, 'off');
//      isProject += '.' + document.dta_form.project[i].value;
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
}

function clear_list(listObj) {
  /* -AO- Clears the listObj list of all selections.
   */
  for (var i = 0; i < listObj.length; i++) {
    listObj[i].selected = false;
  }
}

function embed_it(call_obj, url) {
  /* -AO- Embeds an object within r_body of r_content using either an iFrame
   *      or jQuery's load function, depending on whether or not the calling
   *      object's id is "archive" for the Archive Summary, which is handled
   *      as a special case with jQuery's load().
   */
  var datastr = '';
  $('#r_pop').html('');

  if (call_obj.id == 'archive') {
    $.get(url, function(data) {
      datastr = data;
      datastr = strip_results(datastr, call_obj);
      $('#r_body').html(datastr);
    });
  } else {
    $('#r_body').html('<iframe src="' + url + '" style="width: 100%; height: 100%" scrolling="auto" marginwidth="5" marginheight="5" frameborder="0" vspace="0" hspace="0"></iframe>');
  }
}

function open_win(url) {
  /* -AO- Opens url in a new window rather than a new tab in the browser.
   */
  var new_win = window.open(url, '', 'location=0,scrollbars=1,width=900,height=700');
}

function reset_opt() {
  /* -AO- Resets the search form.
   */

/*
 * Not needed for DBO
 *
  for (var i = 0; i < document.dta_form.project.length; i++) {
    document.dta_form.project[i].checked = false;
  }
*/

  if (document.dta_form.pi != undefined) {
    for (var i = 0; i < document.dta_form.pi.length; i++) {
      document.dta_form.pi[i].selected = false;
    }
  }

  if (document.dta_form.cruise != undefined) {
    for (var i = 0; i < document.dta_form.cruise.length; i++) {
      document.dta_form.cruise[i].selected = false;
    }
  }

  for (var i = 0; i < document.dta_form.subject.length; i++) {
    document.dta_form.subject[i].selected = false;
  }

  build_results();
}

function handle_off_opts(index) {
  var chk_seq = '';

  for (var i = 0; i < document.dta_form.project.length; i++) {
//    if ( !(document.dta_form.project[i].checked) && (document.dta_form.project[i].value != document.dta_form.project[index].value) ) {
      // If not checked AND it is not the project we were trying to turn off... if BSIERP (true) and BSIERP != BSIERP (true)
    if (document.dta_form.project[i].checked) {
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

function set_opts(index, opt) {
  /* -AO- Sets the options in the multiple selection lists to enabled or
   *      disabled depending on if the project is checked.
   */
  var prj_class = '.' + document.dta_form.project[index].value.toLowerCase();

  if ( opt == 'off' ) {
    $(prj_class).each(function() {
      if (!(  ($(this).hasClass('best')) && ($(this).hasClass('bsierp'))  )) {
        $(this).attr('disabled', 'disabled'); // If this element does not have both classes, disable it.
        this.selected = false; // If this element is selected and does not have both classes, unselect it.
      } else {
        handle_off_opts(index);
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

  return newStr;
}

function strip_sp(str) {
  /* -AO- Local method that strips spaces from a given string.
   */
  var fullStr = '';

  fullStr = str.replace(/ /gi,"+");

  return fullStr;
}

function lastname_firstname(str) {
  var lastfirstStr = '';
  var nameLength = 0;
  var name = str.split(' ');
        
  if (name.length > 1) {
    nameLength = name.length;
    lastfirstStr = name[nameLength-1] + ', ';
    
    for (var j = 0; j < nameLength-1; j++) {
      lastfirstStr += name[j];
      
      if (j < nameLength-2) {
        lastfirstStr += ' ';
      }
    }
  }
  
  return lastfirstStr;
}

function hideObj(obj) {
  obj.style.display = 'none';
}

function buildSimpleOptions(optionList) {
  for (var i = 0; i < optionList.length; i++) {
    document.write("<option value=\""+optionList[i]+"\">"+optionList[i]+"</option>");
  }
}
