$(document).ready(function() {
	var browser = navigator.UserAgent;
	
	if ( !/like Gecko$/.test(navigator.userAgent) ) {
		// If the browser is not Internet Explorer, enable the collapsed sections.
		$(".section-title").click(function(event) {
			event.preventDefault();
			
			var arrow = $(this).children("span");
			var section = $(this).next(".section-body");
			
			// Swap the "expand-collapse" arrow classes
			if ( $(arrow).hasClass("arrow-right") ) {
				$(arrow).addClass("arrow-down");
				$(arrow).removeClass("arrow-right");
			} else if ( $(arrow).hasClass("arrow-down") ) {
				$(arrow).addClass("arrow-right");
				$(arrow).removeClass("arrow-down");
			}
			
			// Toggle the hide class on the section's body.
			$(section).toggleClass("hide");
		});
	} else {
		// If the browser is Internet Explorer, disable the collapsed sections.
		$(".section-body").each(function() {
			$(this).removeClass("hide");
		});
		
		// Remove the "expand-collapse" arrow classes but add a right-side checkbox for IE users.
		$(".section-title span.arrow-right").each(function() {
			$(this).html("<input class=\"right\" type=\"checkbox\" />");
			$(this).removeClass("arrow-right");
		});
		
		$(".section-title").hover(function(event) {
			event.preventDefault();
			
			$(this).css("cursor", "default");
		});
		
		$("#note").html("Click on the section checkboxes below to mark each individual section as completed.");
	}
	
	// Implement e-mail click event.
	$(".email").click(function() {
		var email = $(this).html();
		
		email = email.replace(" (at) ", "@");
		email = email.replace(" (dot) ", ".");
		
		window.location.href = "mailto:" + email;
	});
	
	// Implement the title-checker tool.
	$(".section-body #title-checker input[name=title]").keyup(function() {
		var limit = 255; // Change this as the guidelines change.
		var output = $(".section-body #title-checker #title-length");
		var length = $(this).val().length;
		
		if (length > 0) {
			$(output).html(length + " characters");
			
			if (length <= limit) {
				$(output).css("color", "#595"); // Green for good
			} else {
				$(output).css("color", "#f00"); // Red for over the limit
			}
		} else {
			$(output).html("");
		}
		
		// Mark any unacceptable characters in the title.
		var marked = ($(this).val()).replace(/([^a-zA-Z0-9\s\$\-_\.!\*'\(\),])/g, "<span class='err'>$1</span>");
		if ( (($(this).val()).match(/([^a-zA-Z0-9\s\$\-_\.!\*'\(\),])/g)) != null ) {
			$("#title-checker span[id=title-errors]").html("<fieldset><legend>Invalid Title:</legend>" + marked + "</fieldset>");
		} else {
			$("#title-checker span[id=title-errors]").html("");
		}
	});
	
	
	// Comment out this section if you prefer all sections initially collapsed.
	/* *
	$(".section-body").each(function() {
		$(this).removeClass("hide");
	});
	$(".section-title span.arrow-right").each(function() {
		$(this).addClass("arrow-down");
		$(this).removeClass("arrow-right");
	});
	* */
	$(".section-title").filter(":first").click();
});
