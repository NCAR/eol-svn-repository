<%@ page import="meta.HorizontalResolution" %>



<div class="fieldcontain ${hasErrors(bean: horizontalResolutionInstance, field: 'scale', 'error')} required">
	<label for="scale">
		<g:message code="horizontalResolution.scale.label" default="Scale" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="scale" required="" value="${horizontalResolutionInstance?.scale}"/>
</div>

