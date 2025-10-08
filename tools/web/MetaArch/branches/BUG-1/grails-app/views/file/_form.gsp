<%@ page import="meta.File" %>



<div class="fieldcontain ${hasErrors(bean: fileInstance, field: 'index', 'error')} required">
	<label for="index">
		<g:message code="file.index.label" default="Index" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="index" min="0" required="" value="${fieldValue(bean: fileInstance, field: 'index')}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: fileInstance, field: 'name', 'error')} ">
	<label for="name">
		<g:message code="file.name.label" default="Name" />
		
	</label>
	<g:textField name="name" maxlength="255" value="${fileInstance?.name}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: fileInstance, field: 'directory', 'error')} ">
	<label for="directory">
		<g:message code="file.directory.label" default="Directory" />
		
	</label>
	<g:textField name="directory" value="${fileInstance?.directory}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: fileInstance, field: 'fileType', 'error')} ">
	<label for="fileType">
		<g:message code="file.fileType.label" default="File Type" />
		
	</label>
	<g:select name="fileType" from="${meta.FileType?.values()}" keys="${meta.FileType.values()*.name()}" value="${fileInstance?.fileType?.name()}" noSelection="['': '']"/>
</div>

<div class="fieldcontain ${hasErrors(bean: fileInstance, field: 'format', 'error')} ">
	<label for="format">
		<g:message code="file.format.label" default="Format" />
		
	</label>
	<g:select id="format" name="format.id" from="${meta.Format.list()}" optionKey="id" value="${fileInstance?.format?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

<div class="fieldcontain ${hasErrors(bean: fileInstance, field: 'size', 'error')} required">
	<label for="size">
		<g:message code="file.size.label" default="Size" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="size" required="" value="${fieldValue(bean: fileInstance, field: 'size')}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: fileInstance, field: 'dataset', 'error')} required">
	<label for="dataset">
		<g:message code="file.dataset.label" default="Dataset" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="dataset" name="dataset.id" from="${meta.Dataset.list()}" optionKey="id" required="" value="${fileInstance?.dataset?.id}" class="many-to-one"/>
</div>

