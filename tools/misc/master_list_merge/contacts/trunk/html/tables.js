
	function collectIds() {
		var authors = document.getElementById("author-check").checked;
		var idArr = new Array();
		var i = 0;
		
		var allIds = document.getElementById("ds-input").value;
		//console.log(allIds);
		var project = document.getElementById("prj-input").value;
		
		allIds = allIds.replace(/\s+/, "");
		idArr = allIds.split(/,\s*/);
		
		var url = "http://dmg.eol.ucar.edu/cgi-bin/contacts/dataset_contacts.pl?";
		
		if (allIds.match(/^\s*$/) && project.match(/^\s*$/)) {
			return "";
		}
		
		for (var i = 0; i < idArr.length; i++) {
			var s = 'id=';
			
			if (i > 0) {
				s = '&' + s;
			}
			
			url += s + idArr[i];
		}
		
		url += "&authors="+authors;
		
		if (project != "" && allIds == "") {
			url += "&project="+project;
		}
		
		//console.log(url);
		
		return url;
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
		
				getHashes(); // Make sure to get the URL hashes and parse them!
			},
			error: function (xhr, ajaxOptions, thrownError) {
				// Most likely going to be a syntax error - poorly formatted JSON.
				console.log(xhr.status);
				console.log(thrownError);
			}
		});
	}
	
	function getHashes() {
		var hash = window.location.hash;
		if (hash !== "") {
			hash = hash.substring(1,hash.length);
			hash = hash.split("&");
			
			for (var i = 0; i < hash.length; i++) {
				var subHash = hash[i].split('=');
			
				switch( subHash[0] ) {
					case "project":
						$("select#prj-input option[value="+subHash[1]+"]").attr("selected", "selected");
						break;
					case "authors":
						if (subHash[1] == "off") {
							$("input#author-check").removeAttr('checked');
						}
						break;
				}
			}
			
			$("input#find-btn").click();
		}
	};
	
	function setSizes() {
		$('table#ds-tbl th').each(function() {
			var headClass = $(this).attr('class');
			var colClass = headClass.replace('h','c');
			var hw = $(this).width();
			
			if (headClass != 'h3') {
				$('td.'+colClass).each(function() {
					$(this).width(hw);
				});
				$('div.'+headClass).each(function() {
					$(this).width(hw);
				});
			}
		});
		
		$('.c3').width($('.c3').width()+20);
		$('.h3').each(function() {
			$(this).width($('.c3').width());
		});
		
		var theHeight = 135;
		$('.full').height($('.full_box').height() + theHeight);
	}
	
	function updateCellData() {
		var datasetJSONurl = collectIds(); // Fetch all dataset IDs for JSON
		//console.log(datasetJSONurl);
		
		if (datasetJSONurl == "") {
			alert("You didn't enter in a data set archive ID or project!");
			return;
		}
	
	 	$.ajax({
			type: "GET",
			url: datasetJSONurl,
			dataType: 'json',
			success: function(data) {
				var datasets = data;
				var roles = {};
				roles["resourceProvider"] = "Resource Provider";
				roles["custodian"] = "Custodian";
				roles["owner"] = "Owner";
				roles["user"] = "User";
				roles["distributor"] ="Distributor";
				roles["originator"] ="Originator";
				roles["pointOfContact"] ="Point of Contact";
				roles["grantContact"] ="Grant Contact";
				roles["principalInvestigator"] ="Principal Investigator";
				roles["processor"] ="Processor";
				roles["publisher"] ="Publisher";
				roles["author"] ="Author";
				roles["EOL_internal"] ="EOL Internal Contact";
				roles["N/A"] = "N/A";
				//console.log(datasets);
				
				$(".ds-div").each(function() {
					if ($(this).attr('id') != "ds-div-0") {
						$(this).remove();
					}
				});
				
				for (var i = 0; i < datasets.length; i++) {
					var id = datasets[i].archiveId;
                                        var title = datasets[i].title;
                                        var visible = datasets[i].visible;
					
					var clone = $("#ds-div-0").clone();
					
					clone.attr("id", "ds-div-"+(i+1));
					clone.find("h3").html(id);
					clone.find("h3").append(" " + title);
                                        if (visible == 0) {
					   clone.find("h3").append(" " + "(HIDDEN)");
                                        }
					clone.find("#ds-auth").html(datasets[i].authors);
					
					clone.find("tr[id$=tr-0]").attr("id", "ds-"+(i+1)+"-tr-0");
					
					for (var j = 0; j < datasets[i].contacts.length; j++) {
						var trClone;
						
						if (j == 0) {
							trClone = clone.find("tr[id$=tr-0]");
						} else {
							trClone = clone.find("tr[id$=tr-0]").clone();						
							trClone.attr("id", "ds-"+(i+1)+"-tr-"+j);
						}
						
						//console.log(roles[datasets[i].contacts[j].iso_role]);
						trClone.find("td:eq(1)").html(datasets[i].contacts[j].name);
						trClone.find("td:eq(2)").html(datasets[i].contacts[j].org);
						trClone.find("td:eq(0)").html(roles[datasets[i].contacts[j].iso_role]);
					
						if (j > 0) {
							trClone.find("td:eq(0)").removeClass("c1");
							trClone.find("td:eq(1)").removeClass("c2");
							trClone.find("td:eq(2)").removeClass("c3");
						}
					
						
						trClone.css("visibility", "visible");
						trClone.css("display", "table-row");
						
						clone.find("#ds-tbl").append(trClone);
						
					}
					
					clone.css("visibility", "visible");
					clone.css("display", "block");
					
					$("#ds-list").append(clone);
				}
				
				// Color all the updated cells again.
				//setSizes();
			},
			error: function (xhr, ajaxOptions, thrownError) {
				// Most likely going to be a syntax error - poorly formatted JSON.
				console.log(xhr.status);
				console.log(thrownError);
			}
		});
	}


	$(document).ready(function() {
		$('table#ds-tbl tr').hover(
			function() {
				$(this).css('background-color', '#b7ebf0');
				$(this).children('td').css('font-weight', 'bold');
			}, function() {
				$(this).css('background-color', '#fff');
				$(this).children('td').css('font-weight', 'normal');
			}
		);
	});
