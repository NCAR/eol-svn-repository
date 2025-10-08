$(function() {
	var filename = $( "#filename" ),
			ftype = $( "input[name=ftype]" ),
			dopt = $( "input[name=dopt]" ),
			allFields = $( [] ).add( filename ).add( ftype ).add( dopt ),
			tips = $( ".validateTips" );

	function updateTips( t ) {
		tips.text( t ).addClass( "ui-state-highlight" );
		setTimeout(function() {
			tips.removeClass( "ui-state-highlight", 1500 );
		}, 500 );
	}

	function checkLength( o, n, min, max ) {
		if ( o.val().length > max || o.val().length < min ) {
			o.addClass( "ui-state-error" );
			updateTips( "Length of " + n + " must be between " +
				min + " and " + max + "." );
			return false;
		} else {
			return true;
		}
	}

	function checkRegexp( o, regexp, n ) {
		if ( !( regexp.test( o.val() ) ) ) {
			o.addClass( "ui-state-error" );
			updateTips( n );
			return false;
		} else {
			return true;
		}
	}
	
	function checkOptions( o, n ) {
		if ( !( o.is(":checked") ) ) {
			o.addClass( "ui-state-error" );
			updateTips( "One of the " + n + "s must be selected." );
			return false;
		} else {
			return true;
		}
	}

	$( "#dialog-form" ).dialog({
		autoOpen: false,
		height: 300,
		width: 350,
		modal: true,
		buttons: {
			"Create Readme": function() {
				var bValid = true;
				allFields.removeClass( "ui-state-error" );
				
				// Call all the validation functions
				bValid = bValid && checkLength( filename, "filename", 3, 255 ); // filename length is max of 255 characters
				bValid = bValid && checkRegexp( filename, /^[a-zA-Z]([0-9a-zA-Z_-])+$/i, "Filename may consist of letters, numbers, underscores, dashes, and must begin with a letter." );
				bValid = bValid && checkOptions( ftype, "file type" );
				bValid = bValid && checkOptions( dopt, "download option" );

				if ( bValid ) {
					// Add all the download options to the form being sent
					allFields.each(function() {
						if ( $(this).is(":not([type=checkbox],[type=radio])") ) {
							$("form[name!=readme] fieldset.form").append( $(this) );
						} else {
							var allChecked = $(this).filter(":checked");
							$(allChecked).each(function() {
								$(this).attr("checked", "checked");
								$("form[name!=readme] fieldset.form").append( $(this) );
							});
						}
					});
					
					$( this ).dialog( "close" );
					// Trigger the dataSave / docWizardDownload action
					$( "#create-readme-form" ).submit();
					
					// $( this ).dialog( "close" );
				}
			},
			Cancel: function() {
				allFields.val( "" ).removeClass( "ui-state-error" );
				$( this ).dialog( "close" );
			}
		},
		close: function() {
			
		}
	});

	/*
	$( "#create-readme" ).button().click(function() {
		$( "#dialog-form" ).dialog( "open" );
	});
	*/

	$(function() {
		$( "#filetypes" ).buttonset();
		$( "#doptions" ).buttonset();
	});
});

function triggerDialog() {
	return $( "#dialog-form" ).dialog( "open" );
}