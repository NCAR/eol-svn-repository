<%@ page import="meta.Platform" %>



<div class="fieldcontain ${hasErrors(bean: platformInstance, field: 'name', 'error')} ">
	<label for="name">
		<g:message code="platform.name.label" default="Name" />
		
	</label>
	<g:textField name="name" maxlength="255" value="${platformInstance?.name}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: platformInstance, field: 'description', 'error')} ">
	<label for="description">
		<g:message code="platform.description.label" default="Description" />
		
	</label>
	<g:textField name="description" value="${platformInstance?.description}"/>
</div>

