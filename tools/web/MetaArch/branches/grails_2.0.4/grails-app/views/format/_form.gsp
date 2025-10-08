<%@ page import="meta.Format" %>



<div class="fieldcontain ${hasErrors(bean: formatInstance, field: 'name', 'error')} required">
	<label for="name">
		<g:message code="format.name.label" default="Name" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="name" required="" value="${formatInstance?.name}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: formatInstance, field: 'description', 'error')} ">
	<label for="description">
		<g:message code="format.description.label" default="Description" />
		
	</label>
	<g:textField name="description" value="${formatInstance?.description}"/>
</div>

