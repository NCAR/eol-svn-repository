<script type="text/javascript">
<%--	var authorChildCount = ${datasetInstance?.authors?.size()} + 0;--%>
	var authorChildCount = $("#authorChildList > div.author-div").size() + 0;
	var datasetId = ${datasetInstance?.id} + 0;
	 
	function addAuthorChild() {
		var authorId = $("#authSearchId").attr("value");
		var authorName = $("#authSearchName").attr("value");
		$("#authorError").html("");

		if (authorId != "" && authorId != null) {
			// Check to see if this author is already in the list.
			var existing = $("input[name^='authors.sortKeys."+authorId+"']");
			if ( existing.size() > 0  ) {
				authorName = authorName.replace( /\s*\(.*\)$/, "");
				$("#authorError").html("Author \""+authorName+"\" is already part of the list.");
				$("#authSearchInput").val("");
				return;
			}
			
			var clone = $("#author_clone").clone();
			var htmlId = "authors.";		
			var deleteIcon = "${resource(dir:'images/skin', file:'database_delete.png')}";
	
			clone.find("input[id^='authors.deleted']")
				.attr('id', htmlId+'deleted.'+authorId)
				.attr('name', htmlId+'deleted.'+authorId);
			clone.find("input[id^='authors.new']")
				.attr('id', htmlId+'new.'+authorId)
				.attr('name', htmlId+'new.'+authorId)
				.attr('value', 'true');
			clone.find("input[id^='authors.sortKeys']")
				.attr('id', htmlId+'sortKeys.'+authorId)
				.attr('name', htmlId+'sortKeys.'+authorId)
				.attr('value', authorChildCount + 1);

			clone.find("span[id$=name]")
				.attr('id', htmlId+'name')
				.html(authorName);
			clone.find("span[id$=sort]")
				.attr('id', htmlId+'sort')
				.html(authorChildCount + 1);
	
			clone.attr('id', 'author'+authorChildCount);
			$("#authorChildList").append(clone);
			clone.show();
			authorChildCount++;
			$("#authSearchInput").val("");
		}
	}

	$(document).on('click', '.del-author', function() {
		// find the parent div
		var parent = $(this).parents(".author-div");
		// get the deleted input
		var delInput = parent.find("input[id^='authors.deleted']");
		// if this is still not persisted...
		var newValue = parent.find("input[id^='authors.new']").attr('value');
		// remove from the DOM!
		if (newValue == 'true') {
			parent.remove();
		} else {
			// set the deleted property to true
			delInput.attr('value', 'true');
			// and hide the parent div
			parent.hide();
		}
	});	
	
	function validateAuthors() {
		$("#authorChildList").children().find("input[id^='authors.new'][value='true']").parent().each (function() {
			// Collects all the author divs where its "new" input is true to test for if it has a author
			// associated with it.  If not, delete it.
			console.log("found: "+$(this).html());
			if ($(this).children("input[id$=href]").attr('value') == '') {
				$(this).children("span.del-author").click();
			}
		});
	}

	$(function() {	
		$("input#authSearchInput").autocomplete({
				minLength: 2,
				position: { my: "right top", at: "right bottom"},
				source: "${createLink(controller:'author', action:'asearch')}",
				select: function(event, ui) {
					// On select, add author to list!
					$("#authSearchId").val(ui.item.value);
					$("#authSearchInput").val(ui.item.label);
					$("#authSearchName").val(ui.item.label);
					},
				open: function(event, ui) { $(".ui-autocomplete").css("z-index", 999); }
		});

		$("#authorChildList").sortable({
			connectWith: "#authorChildList",
			revert: true,
			placeholder: "ui-state-highlight",
			helper: 'clone',
			items: "> div.author-div",
			stop: function(e, ui) {
				var i = 1;
				$("#authorChildList > div.author-div").each(function() {
					$(this).find("span[id$=sort]").html(i);
					$(this).find("input[id^='authors.sortKey']").attr("value", i);
					i++;
				});
			}		
		});

		$("#authorChildList").droppable({
			accept: "#authorChildList > div.author-div",
			activeClass: "ui-state-highlight"
		});
	});
</script>

<div id="authorListdiv" class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'authors', 'error')} ">
	<label for="authorList">
		Author List
	</label>
	
	<div class="tooltip-ui" id="authorIntro">
		Do you work with others to create this data set?  If so, please include them here 
		by clicking on the "Add Author" button below and filling out the provided fields.
	</div>
	
	<div id="authorChildList">
		<g:if test="${datasetInstance?.id != null}">
		<g:each var="author" in="${datasetInstance?.authorList().sort{it.sortKey}}" status="i">
			<g:render template="/author/author" model="['dsAuthor':author,'i':i, 'hidden':false]" />
		</g:each>
		</g:if>
	</div>
	
	<br />
	<span id="authorChildSearch" style="color: #666;">
		Search Authors: 
		<input type="text" id="authSearchInput" class="ui-autocomplete-input" autocomplete="off" role="textbox" />
		<input type="hidden" id="authSearchId" value="" />
		<input type="hidden" id="authSearchName" value="" />
		<input type="button" value="Add Author" onclick="addAuthorChild();" />
		<input type="button" id="create-author" onclick="triggerDialog();" value="Create New Author" />
		<span class="error-span" id="authorError"></span>
	</span>
</div>
<div style="clear: both;"></div>
