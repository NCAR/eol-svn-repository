<div id="file${i}" class='file-div <g:if test="${file}"></g:if><g:else>file-new</g:else>' <g:if test="${hidden}">style="display:none;"</g:if>>
	<g:if test="${file}">
	<span>
		<span id="file${i}-name">${file?.name}</span> (${file?.format}, ${file?.size} kB)
	</span>
	</g:if>
	<g:else><span>New File:</span></g:else>
	<br />
	
	<g:hiddenField name='files.id' value='${file?.id}'/>
	<g:hiddenField name='files.index' value='${i}'/> <!-- to sort these later -->
	<g:hiddenField name='files.deleted' value='false'/>
	<input id="files[${i}].new" type="hidden" value="false" new="files.${i}.new" />
	
	<g:select class="select-fileType" name="files.fileType" from="${meta.FileType?.values()}" keys="${meta.FileType.values()*.name()}" required="" value="${file?.fileType?.name()}"/>
	<g:hiddenField title="File Name" id="files[${i}].name" name="files.name" maxlength="255" value="${file?.name}"/>
	<g:select class="select-format" id="files[${i}].format" name="files.format.id" from="${meta.Format.list()}" optionKey="id" value="${file?.format?.id}" class="many-to-one"/>
	<g:hiddenField class="input-number" title="File size in KB" type="number" name="files.size" value="${file?.size}"/>
	<input type="file" id="files[${i}].file" name="files[${i}].file" onchange="saveFilename(this);" />
	
	<span title="Delete this File from the List" class="del-file">
		<img src="${resource(dir:'images/skin', file:'database_delete.png')}" />
	</span>
</div>