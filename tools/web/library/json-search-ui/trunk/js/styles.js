function getBrowserHeight() {
  var theHeight;
//  var firstHeight;
  
  if (window.innerHeight) { theHeight = window.innerHeight; }
  else if (document.documentElement && document.documentElement.clientHeight) { theHeight = document.documentElement.clientHeight; }
  else if (document.body) { theHeight = document.body.clientHeight; }
  
//  if (firstHeight != theHeight) { console.log("window.innerHeight is: " + firstHeight); }

  //console.log("Current browser height is: " + theHeight);
  
  return theHeight;
}

function getBrowserWidth() {
  var theWidth;
  
  if (window.innerWidth) { theWidth = window.innerWidth; }
  else if (document.documentElement && document.documentElement.clientWidth) { theWidth = document.documentElement.clientWidth; }
  else if (document.body) { theWidth = document.body.clientWidth; }
  
  //console.log("Current browser width is: " + theWidth);
  
  return theWidth;

}

function setFrameSize() {
  var w = getBrowserWidth();
  var h = getBrowserHeight();

  // Adjust the height to not include the header's height
  h = h - document.getElementById('headerwrap').clientHeight;

  $('div.clr_foot').css('height', '0px');
  $('iframe#mapserver_frame').css('height', h+'px');
  $('iframe#mapserver_frame').css('width', w+'px');

  if (/Internet Explorer/.test(navigator.userAgent) || /MSIE/.test(navigator.userAgent)) {
    $('iframe#mapserver_frame').css('min-height', h+'px');
  }

  // If there are scrollbars, adjust the width to not overflow the frame underneath them
  // at this point in time, there will always be a scrollbar-y.
//  if (/^Win/.test(navigator.platform)) {
    w = w - 20;
    $('iframe#mapserver_frame').css('width', w+'px');
//  }
}



/*
 * switchTabs(on_obj, off_obj)
 *
 * Switches the styles for the tabs in top_head and display the appropriate
 * text with the associated tab.
 *
 */
function switchTabs(on_obj) {
  $(on_obj).siblings().css('visibility','hidden');
  $(on_obj).siblings().css('display','none');
  $(on_obj).css('visibility','visible');
  $(on_obj).css('display','block');

  $('.selected').each(function(index) {
    $(this).removeClass('selected').addClass('unselected');
  });

  var call_obj = '#' + on_obj.slice(1,-5) + '_title';

  $(call_obj).removeClass('unselected').addClass('selected');

  if (on_obj == '.archive_text' || on_obj == '.data_policy_text') {
    $('.top_box').css('overflow-y', 'scroll');
  } else {
    $('.top_box').css('overflow-y', 'hidden');
  }
}


/*
 * getWidth()
 *
 * Code adapted from http://www.javascripter.net/faq/browserw.htm
 *
 */
function getWidth() {
  var width;

  if (document.body && document.body.offsetWidth) {
    width = document.body.offsetWidth;
  }
  if (document.compatMode=='CSS1Compat' &&
      document.documentElement &&
      document.documentElement.offsetWidth ) {
    width = document.documentElement.offsetWidth;
  }
  if (window.innerWidth && window.innerHeight) {
    width = window.innerWidth;
  }

  return width;
}


/*  
 * setOffset()
 *
 * Checks the browser's width and reassigns the appropriate offset for 
 * the transparent logos.
 *
 */
function setOffset() {
  var width = getWidth();
  var offsetStart = (width - 900)/2;
  if (offsetStart < 0) { offsetStart *= -1; }
  var offset = offsetStart - 1;
  var lelem = document.getElementById('leftTrans');
  var relem = document.getElementById('rightTrans');

  offset += "px";

//  alert("left_head: " + document.getElementById('left_head'));
//  alert("leftTrans: " + lelem + "\nrightTrans: " + relem);

  lelem.style.left = offset;
  relem.style.right = offset;

  alert("leftTrans: " + lelem.style.left + "\nrightTrans: " + relem.style.right);
}


$(function() {
	$(".head-text").hover(function() {
		$(this).css("cursor", "pointer");
	});
	
	$(".head-text").click(function() {
		$("#nav1 a").click();
	});
});