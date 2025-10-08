<%@ page import="meta.Agency" %>



<div class="fieldcontain ${hasErrors(bean: agencyInstance, field: 'name', 'error')} required">
	<label for="name">
		<g:message code="agency.name.label" default="Name" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="name" required="" value="${agencyInstance?.name}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: agencyInstance, field: 'description', 'error')} ">
	<label for="description">
		<g:message code="agency.description.label" default="Description" />
		
	</label>
	<g:textField name="description" value="${agencyInstance?.description}"/>
</div>

