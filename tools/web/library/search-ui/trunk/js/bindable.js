/*
 * NOTE:
 *	IE 8 does not support console functions, and will break functions it is used in.
 *
 */


$(document).ready( function() {
	/* 
	 * Original Source:
	 * 	http://www.designchemical.com/blog/index.php/jquery/sort-items-alphabetically-using-jquery/ 
	 * Modified to handle multiple column sorting.
	 */
	$('#results').delegate('.link-sort-table', 'click', function(e) {
		if (e.preventDefault) {
			e.preventDefault();
		} else {
			e.returnValue = false;
		}
		var $sort = this;
		var col = (this.id).substring(3);
		var $table = $('#sort-table');
		var $rows = $('tbody > tr',$table);
		$rows.sort(function(a, b){
			var keyA = $('td:eq('+col+')',a).text();
			var keyB = $('td:eq('+col+')',b).text();
			if( ( !($($sort).hasClass('desc')) && !($($sort).hasClass('asc')) ) || ( !($($sort).hasClass('asc')) && $($sort).hasClass('desc') ) ){
//console.log(keyA.toLowerCase()+' > '+keyB.toLowerCase()+'? '+((keyA.toLowerCase() > keyB.toLowerCase()) ? 1 : -1));
				return (keyA.toLowerCase() > keyB.toLowerCase()) ? 1 : -1;
			} else {
//console.log(keyA.toLowerCase()+' < '+keyB.toLowerCase()+'? '+((keyA.toLowerCase() < keyB.toLowerCase()) ? 1 : -1));
				return (keyA.toLowerCase() < keyB.toLowerCase()) ? 1 : -1;
			}
		});
		if ( $($sort).hasClass('asc') && !($($sort).hasClass('desc')) ) {
			$($sort).addClass('desc');
			$($sort).removeClass('asc');
		} else if ( !($($sort).hasClass('asc')) && $($sort).hasClass('desc') ) {
			$($sort).addClass('asc');
			$($sort).removeClass('desc');
		} else {
			$($sort).addClass('asc');
		}
		$.each($rows, function(index, row){
var keyC = $('td:eq('+col+')',row).text();
//console.log(keyC);
		  $table.append(row);
		});
		//e.preventDefault();
	});
	
	$('#results').delegate('.link-sort-table', 'hover', function(e) {
		if (e.preventDefault) {
			e.preventDefault();
		} else {
			e.returnValue = false;
		}
		$(this).css('cursor', 'pointer');
	});
});
