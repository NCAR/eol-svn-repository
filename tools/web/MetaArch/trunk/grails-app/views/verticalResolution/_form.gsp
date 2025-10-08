<%@ page import="meta.VerticalResolution" %>



<div class="fieldcontain ${hasErrors(bean: verticalResolutionInstance, field: 'scale', 'error')} required">
	<label for="scale">
		<g:message code="verticalResolution.scale.label" default="Scale" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="scale" required="" value="${verticalResolutionInstance?.scale}"/>
</div>

