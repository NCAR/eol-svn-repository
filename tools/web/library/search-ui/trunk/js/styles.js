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




$(document).ready(function() {
	/* XS Device Handling for the Sub-menu Dropdowns */
	$('li.dropdown-submenu > a').on('click', function(event) {
		// Avoid following the href location when clicking
		event.preventDefault(); 
		// Avoid having the menu to close when clicking
		event.stopPropagation();
		// If a menu is already open we close it
		$('ul.dropdown-menu [data-toggle=dropdown]').parent().removeClass('open');
		// Opening the menu that was clicked on
		$(this).parent().toggleClass('open');
	});
});