<%@ page import="meta.XmlTemplate" %>



<div class="fieldcontain ${hasErrors(bean: xmlTemplateInstance, field: 'filename', 'error')} ">
	<label for="filename">
		<g:message code="xmlTemplate.filename.label" default="Filename" />
		
	</label>
	<g:textField name="filename" maxlength="255" value="${xmlTemplateInstance?.filename}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: xmlTemplateInstance, field: 'file', 'error')} ">
	<label for="file">
		<g:message code="xmlTemplate.file.label" default="File" />
		
	</label>
	<input type="file" id="file" name="file" />
</div>

<div class="fieldcontain ${hasErrors(bean: xmlTemplateInstance, field: 'body', 'error')} ">
	<label for="body">
		<g:message code="xmlTemplate.body.label" default="Body" />
		
	</label>
	<g:textArea name="body" cols="40" rows="5" value="${xmlTemplateInstance?.body}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: xmlTemplateInstance, field: 'size', 'error')} required">
	<label for="size">
		<g:message code="xmlTemplate.size.label" default="Size" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="size" required="" value="${fieldValue(bean: xmlTemplateInstance, field: 'size')}"/>
</div>

