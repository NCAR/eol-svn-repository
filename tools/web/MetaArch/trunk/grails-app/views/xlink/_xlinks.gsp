<script type="text/javascript">
	var xlinkChildCount = ${datasetInstance?.xlinks?.size()} + 0;
	 
	function addXlinkChild() {
		var clone = $("#xlink_clone").clone();
		var htmlId = "xlinks["+xlinkChildCount+'].';
		var xlinkInput = clone.find("input[id$=href]");		
		var deleteIcon = "${resource(dir:'images/skin', file:'database_delete.png')}";

		clone.find("input[id$=id]")
			.attr('id', htmlId+'id')
			.attr('name', htmlId+'id');
		clone.find("input[id$=deleted]")
			.attr('id', htmlId+'deleted')
			.attr('name', htmlId+'deleted');
		clone.find("input[id$=new]")
			.attr('id', htmlId+'new')
			.attr('name', htmlId+'new')
			.attr('value', 'true');
		clone.find("select[id$=type]")
			.attr('id', htmlId+'type')
			.attr('name', htmlId+'type');
		clone.find("input[id$=title]")
			.attr('id', htmlId+'title')
			.attr('name', htmlId+'title');
		xlinkInput.attr('id', htmlId+'href')
			.attr('name', htmlId+'href');

		clone.attr('id', 'xlink'+xlinkChildCount);
		$("#xlinkChildList").append(clone);
		clone.show();
		xlinkInput.focus();
		xlinkChildCount++;

<%--		--%>
<%--		var templateHtml = "<div class='expandableXlinkList' id='" + htmlId + "' name='" + htmlId + "'>\n";--%>
<%--		var key = '';--%>
<%--		var value = '';--%>
<%----%>
<%--		templateHtml += "<select id='expandableXlinkList[" + xlinkChildCount + "].type' name='xlinksList[" + xlinkChildCount + "].type'>\n";--%>
<%--		<% for ( j in meta.XlinkType.values() ) { %>--%>
<%--		key = '${j.key}';--%>
<%--		value = '${j}';--%>
<%--		templateHtml += "<option value=\""+key+"\">"+value+"</option>";--%>
<%--		<% } %>--%>
<%--		templateHtml += "</select>";--%>
<%--		templateHtml += "<input type='text' title='URL Title' id='expandableXlinkList[" + xlinkChildCount + "].title' name='xlinksList[" + xlinkChildCount + "].title' />\n";--%>
<%--		templateHtml += "<input type='text' title='URL Href' id='expandableXlinkList[" + xlinkChildCount + "].href' name='xlinksList[" + xlinkChildCount + "].href' />\n";--%>
<%--		templateHtml += "<span onClick='$(\"#" + htmlId + "\").remove();'><img src='" + deleteIcon + "' /></span>\n";--%>
<%--		templateHtml += "</div>\n";--%>
<%--		--%>
<%--		$("#xlinkChildList").append(templateHtml);--%>
<%--		xlinkChildCount++;--%>
	}

	$('.del-xlink').live('click', function() {
		// find the parent div
		var parent = $(this).parents(".xlink-div");
		// get the deleted input
		var delInput = parent.find("input[id$=deleted]");
		// if this is still not persisted...
		var newValue = parent.find("input[id$=new]").attr('value');
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
	
	function validateXlinks() {
		$("#xlinkChildList").children().find("input[id$=new][value='true']").parent().each (function() {
			// Collects all the xlink divs where its "new" input is true to test for if it has a xlink
			// associated with it.  If not, delete it.
			console.log("found: "+$(this).html());
			if ($(this).children("input[id$=href]").attr('value') == '') {
				$(this).children("span.del-xlink").click();
			}
		});
	}
</script>

<div id="xlinkListdiv" class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'xlinks', 'error')} ">
	<label for="xlinkList">
		Data Set URL List
	</label>
	
	<span id="xlinkChildTotal" style="float: right; color: #666;">
		Total # Links: <strong>${datasetInstance.xlinks.size()}</strong>
	</span>
	
	<div class="tooltip" id="xlinkIntro">
		Do you have an off-site data set?  Can additional information regarding 
		your data be found at a specific web page?  If so, please include it here 
		by clicking on the "Add URL" button below and filling out the provided fields.
	</div>
	
	<div id="xlinkChildList">
		<g:each var="xlink" in="${datasetInstance.xlinks}" status="i">
			<g:render template="/xlink/xlink" model="['xlink':xlink,'i':i, 'hidden':false]" />
		</g:each>
	</div>
	<input type="button" value="Add URL" onclick="addXlinkChild();" />
</div>
<div style="clear: both;"></div>
