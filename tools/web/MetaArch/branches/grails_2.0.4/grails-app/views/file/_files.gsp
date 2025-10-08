<script type="text/javascript">
	var fileChildCount = ${datasetInstance?.files?.size()} + 0;
	 
	function addFileChild() {
		var clone = $("#file_clone").clone();
		var htmlId = "files["+fileChildCount+'].';
		//var htmlName = "files."+fileChildCount+'.';
		//var htmlName = "files["+fileChildCount+'].';
		var htmlName = "files.";
		var fileInput = clone.find("input[type=file]");		
		var deleteIcon = "${resource(dir:'images/skin', file:'database_delete.png')}";

		clone.find("input[id$=id]")
			.attr('id', htmlId+'id')
			.attr('name', htmlName+'id');
		clone.find("input[id$=index]")
			.attr('id', htmlId+'index')
			.attr('value', fileChildCount)
			.attr('name', htmlName+'index');
		clone.find("input[id$=deleted]")
			.attr('id', htmlId+'deleted')
			.attr('name', htmlName+'deleted');
		clone.find("input[id$=new]")
			.attr('id', htmlId+'new')
			.attr('new', htmlId+'new')
			.attr('value', 'true');
		clone.find("select[id$=fileType]")
			.attr('id', htmlId+'fileType')
			.attr('name', htmlName+'fileType');
		clone.find("input[id$=title]")
			.attr('id', htmlId+'title')
			.attr('name', htmlName+'title');
		clone.find("input[id$=name]")
			.attr('id', htmlId+'name')
			.attr('name', htmlName+'name');
		clone.find("select[id$=format]")
			.attr('id', htmlId+'format')
			.attr('name', htmlName+'format.id');
		clone.find("input[id$=size]")
			.attr('id', htmlId+'size')
			.attr('value', 0)
			.attr('name', htmlName+'size');
		fileInput.attr('id', htmlId+'file')
			.attr('name', htmlId+'file');

		clone.attr('id', 'file'+fileChildCount);
		$("#fileChildList").append(clone);
		clone.show();
		fileInput.focus();
		fileChildCount++;
	}

	$('.del-file').live('click', function() {
		// find the parent div
		var parent = $(this).parents(".file-div");
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
	
	function saveFilename(obj) {
		var fullPath = obj.value;
		var findex = obj.id;
		var fname = "";
		var extension = "";

		findex = findex.replace("files[", "");
		findex = findex.replace("].file", "");

		if (fullPath != "") {
			var startIndex = (fullPath.indexOf('\\') >= 0 ? fullPath.lastIndexOf('\\') : fullPath.lastIndexOf('/'));
			fname = fullPath.substring(startIndex);
			if (fname.indexOf('\\') === 0 || fname.indexOf('/') === 0) {
				fname = fname.substring(1);
			}
		} else {
			fname = document.getElementById("file"+findex+"-name").innerHTML;
		}		

		document.getElementById("files["+findex+"].name").value = fname;

		// If you can find the file extension in the list, set it!
		extension = fname.substring(fname.lastIndexOf('.'));
		extension = extension.replace(".", "");
		extension = extension.toUpperCase();

		var formatList = document.getElementById("files["+findex+"].format");
		var foundFormat = false;
		var defaultFormat = 0;
		for (var i = 0; i < formatList.length; i++) {
			if (formatList.options[i].innerHTML == extension && !formatList.options[i].selected) {
				formatList.options[i].selected = "selected";
				foundFormat = true;
			} else if (formatList.options[i].innerHTML != extension && formatList.options[i].selected) {
				formatList.options[i].selected = null;
			}
			if (formatList.options[i].innerHTML.toLowerCase() == "unknown") {
				defaultFormat = i;
			}
		}

		if (!foundFormat) {
			formatList.options[defaultFormat].selected = "selected";
		}
	}

	function validateFiles() {
		$("#fileChildList").children().find("input[id$=new][value='true']").parent().each (function() {
			// Collects all the file divs where its "new" input is true to test for if it has a file
			// associated with it.  If not, delete it.
			if ($(this).children("input[name='files.name']").attr('value') == '') {
				$(this).children("span.del-file").click();
			}
		});

		$("#fileListTotalField").attr("value", $("#fileChildList div.file-div").length);
	}
</script>

<div class="fieldcontain ${hasErrors(bean: datasetInstance, field: 'files', 'error')} ">
	<label for="fileList">
		Data Set File List
	</label>
	
	<span id="fileChildTotal" style="float: right; color: #666;">
		Total # Files: <strong>${datasetInstance.files.size()}</strong>
	</span>	
	
	<div id="fileChildList" style="padding: 10px 0px;">
		<g:each var="file" in="${datasetInstance.files}" status="i">
			<g:render template="/file/file" model="['file':file,'i':i]" />
		</g:each>
		<br />
	</div>
	<input type="button" value="Add File" onclick="addFileChild();" />
	<input type="button" value="Remove empty/unused File(s)" onclick="validateFiles();" />
	<g:hiddenField id="fileListTotalField" name="fileListTotal" value="${datasetInstance.files.size()}" />
	
<%--	<span id="fileChildTotal" style="float: right; color: #666;">--%>
<%--		Total # Files: <strong>${datasetInstance.files.size()}</strong>--%>
<%--	</span>--%>
</div>
<div style="clear: both;"></div>

