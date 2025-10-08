Array.prototype.contains = function ( value ) {
	for (i in this) {
		if (this[i] == value) {
			return true;
		}
	}
	return false;
}

Array.prototype.convertAndJoin = function ( convertMap, separator ) {
	var newArr = new Array();
	
	for (i in this) {
		if (this[i] != "" && this[i] != null) {
			newArr.push( convertMap[this[i]] );
		}
	}
	
	return newArr.join(separator);
}




var data; // Used to load the entire JSON file at document.ready
var sites = {"site1": "Southern St. Lawrence Island", "site2": "Chirikov Basin", "site3": "Southern Chukchi Sea", "site4": "Central Chukchi Sea", "site5": "Barrow Canyon"};


function buildResultsTable( results ) {
	var table;
	var row = "";
	var tsects = new Array();
	
	if ( results.length > 0 ) {
		$(".right-panel #results").html("<table></table>");
		table = $(".right-panel #results table");
		
		row = "<tr class='top'><th>Dataset Title</th><th>Author/Researcher</th>";
		row += "<th>Transect</th><th>Vessel</th><th>Keywords</th></tr>";
		
		table.append(row);
	} else {
		$(".right-panel #results").html("<h3>No Results Found.</h3>");
		return;
	}
	
	for (var i = 0; i < results.length; i++) {
		tsects = jQuery.makeArray(results[i]["search"]["transect"]);
		
		if ( i%2 == 0 ) {
			row = "<tr class='odd'>";
		} else {
			row = "<tr class='even'>";
		}
		
		row += "<td>" + results[i]["title"] + "</td>";
		row += "<td>" + (results[i]["search"]["researcher"]).join(", ") + "</td>";
		row += "<td>" + tsects.convertAndJoin(sites, "<br />") + "</td>";
		row += "<td>" + results[i]["search"]["vessel"] + "</td>";
		row += "<td>" + (results[i]["search"]["keywords"]).join(", ") + "</td>";
		row += "</tr>";
		
		table.append(row);
	}
}

function filterObjects( filters ) {
	var filtered = new Array();
	
	var transect = new Array();
	var researcher = new Array();
	var year = new Array();
	var month = new Array();
	
	for (var i = 0; i < data.length; i++) {
		if (filters.length > 0) {
			for (var j = 0; j < filters.length; j++) {
			
				transect = jQuery.makeArray( data[i]["search"]["transect"] );
				researcher = jQuery.makeArray( data[i]["search"]["researcher"] );
				year = jQuery.makeArray( data[i]["search"]["year"] );
				month = jQuery.makeArray( data[i]["search"]["month"] );
			
				if ( transect.contains(filters[j]) || 
					 data[i]["search"]["vessel"] == filters[j] || 
					 researcher.contains(filters[j]) || 
					 year.contains(filters[j]) || 
					 month.contains(filters[j]) ) {
					
					filtered.push(data[i]);
					break; // Leave the filters loop to move on to the next object
				}
			}
		} else {
			filtered.push(data[i]); // If there are no filters, push all data
		}
	}
	
	return filtered;
}

function loadAllResults() {
	// Filter the JSON to selected results
	var filters = new Array();
	var results = filterObjects(filters);
	
	// Build the JSON results as HTML
	buildResultsTable(results);
}


$(function() {
	// Retrieve the JSON
	var jsonFilePath = "../html/datamap.json"; // Change to reflect path
	$.getJSON( jsonFilePath, function( json ) {
		data = json;
		loadAllResults(); // Load all results on document load.
	});

	$(".section").click(function() {
		if ( $(this).children(".collapse").html() == "+" ) {
			$(this).children(".collapse").html("-");
			$(this).next(".section-body").removeClass("section-hide");
		} else {
			$(this).children(".collapse").html("+");
			$(this).next(".section-body").addClass("section-hide");
		}
	});
	
	$(".section-clear").click(function() {
		var total = $(this).siblings("input:checked").size();
		var i = 1;
		
		$(this).siblings("input:checked").each(function () {
			if ( i >= total ) {
				$(this).click(); // Triggers the reset filtering
			} else {
				// Just uncheck the box (the last checked box triggers)
				$(this).attr("checked", false);
				i++;
			}
		});
	});
	
	$(".section-body input[type=checkbox]").click(function() {
		var filters = new Array();
		var allChecked = $(".section-body input[type=checkbox]").filter(":checked");
		
		$(".right-panel #filter ul").html("");
		var name = ""
		var exitButton = "<span class=\"exit fa fa-times-circle\"></span>";
		allChecked.each(function() {
			name = $(this).val();
			$(".right-panel #filter ul").append("<li>"+name+exitButton+"</li>");
			
			// Add to the search JSON filters
			if ( /site/.test($(this).attr("name")) ) {
				filters.push( $(this).attr("name") );
			} else {
				filters.push( name );
			}
		});
		
		// Filter the JSON to selected results
		var results = filterObjects(filters);
		
		// Build the JSON results as HTML
		buildResultsTable(results);
	});
	
	$(".exit").click(function() {
		console.log("booyah");
		var filter = $(this).parent().html();
		filter = filter.replace("<span class=\"exit fa fa-times-circle\"></span>", "");
		
		var input = $(".section-body input[type=checkbox]").filter("[value^='"+filter+"']");
		
		input.click();
	});
});
