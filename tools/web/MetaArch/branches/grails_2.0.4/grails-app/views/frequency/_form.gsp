<%@ page import="meta.Frequency" %>



<div class="fieldcontain ${hasErrors(bean: frequencyInstance, field: 'name', 'error')} ">
	<label for="name">
		<g:message code="frequency.name.label" default="Name" />
		
	</label>
	<g:textField name="name" maxlength="255" value="${frequencyInstance?.name}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: frequencyInstance, field: 'sortKey', 'error')} required">
	<label for="sortKey">
		<g:message code="frequency.sortKey.label" default="Sort Key" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="sortKey" required="" value="${fieldValue(bean: frequencyInstance, field: 'sortKey')}"/>
</div>

