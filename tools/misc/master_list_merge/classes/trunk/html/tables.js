function loadClasses(tableType) {
	var url = "http://dmg.eol.ucar.edu/cgi-bin/ml_merge/ml_class_list.pl?type="+tableType;
	// Append the table type - classification, category, or platform?
	// Default type is classification.
	
	$.ajax({
		type: "GET",
		url: url,
		dataType: 'html',
		success: function(data) {
			$("#classes").html(data);
			$("#json-link").attr("href", "http://dmg.eol.ucar.edu/cgi-bin/ml_merge/ml_class_json.pl?type="+tableType);
			
			$(".parent").each(function() {
				$(this).children().each(function() {
					if (!$(this).hasClass("right-span")) {
						$(this).toggle();
					}
				});
			});
			
			
	
			$(".parent").bind("click", function(e) {
				e.stopPropagation(); // Prevents click event from toggling parents of parent class elements
				
				if ( $(e.target).is("li") || $(this).children("ul").css("display") == "none" ) {
					var rgb = ($(".parent").css("background-color")).replace(/^rgba?\(|\s+|\)$/g,'').split(',');
					for (var i = 0; i < rgb.length; i++) {
						rgb[i] = Math.floor(rgb[i] * 0.85);
					}
					
					var defaultColor = $(".parent").css("background-color");
					var clickColor = "rgb("+rgb.join()+")";
					
					$(this).children().each(function() {
						//console.log($(this).prop("tagName")+" has parent?\t "+$(this).children().hasClass("parent"));
						if ($(this).children().hasClass("parent")) {
							if ($(this).children(".parent").css("background-color") == defaultColor) {
								$(this).children(".parent").css("background-color", clickColor);
							} else {
								$(this).children(".parent").css("background-color", defaultColor);
							}
						}
						
						if (!$(this).hasClass("right-span")) {
							$(this).toggle();
						}
					});
				}
			});
			
			$(".parent").bind("mouseenter mouseleave", function() {
				$(this).css("cursor", "pointer");
			});
			
			
		},
		error: function (xhr, ajaxOptions, thrownError) {
			console.log(xhr.status);
			console.log(thrownError);
		}
	});
	
	/*
	url = "http://dmg.eol.ucar.edu/cgi-bin/ml_merge/ml_class_json.pl";
	$.ajax({
		type: "GET",
		url: url,
		dataType: 'html',
		success: function(data) {
			$("#classesJson").html('<pre class="jsonCode">'+data+'</pre>');
		},
		error: function (xhr, ajaxOptions, thrownError) {
			// Most likely going to be a syntax error - poorly formatted JSON.
			console.log(xhr.status);
			console.log(thrownError);
		}
	});
	*/
}

function loadProjects() {
	//prj-input
	var url = "http://dmg.eol.ucar.edu/cgi-bin/contacts/dataset_contacts.pl?all=true";
	
	$.ajax({
		type: "GET",
		url: url,
		dataType: 'json',
		success: function(data) {
			var prjData = data;
			var projects = "";
			
			for (var i = 0; i < prjData.projects.length; i++) {
				var name = prjData.projects[i].name;
				
				projects += "<option value=\"" + name + "\">" + name + "</option>\n";
			}
			//console.log(projects);
			
			$("#prj-input").html(projects);
		},
		error: function (xhr, ajaxOptions, thrownError) {
			// Most likely going to be a syntax error - poorly formatted JSON.
			console.log(xhr.status);
			console.log(thrownError);
		}
	});
}

function showDsList(obj, tableType, title, id) {
	var spanParent = $(obj).parent();
	
	$(".selected-item").each(function() {
		$(this).removeClass("selected-item");
	});
	
	$(spanParent).addClass("selected-item");
	
	var url = "http://dmg.eol.ucar.edu/cgi-bin/ml_merge/ds_list.pl?id="+id+"&type="+tableType;
	console.log(url);
	
	$.ajax({
		type: "GET",
		url: url,
		dataType: 'html',
		success: function(data) {
			console.log(data);
			$("#ds-zfloat > #ds-list").html(data);
			$("#ds-zfloat #ds-type").html(tableType+": "+title);
			
			$("#ds-zfloat").show();
		},
		error: function (xhr, ajaxOptions, thrownError) {
			console.log(xhr.status);
			console.log(thrownError);
		}
	});
}
function hideDsList() {
	$("#ds-zfloat > #ds-list").html("");
	$("#ds-zfloat").hide();
	
	$(".selected-item").each(function() {
		$(this).removeClass("selected-item");
	});
}


$(document).ready(function() {
	loadClasses();
});