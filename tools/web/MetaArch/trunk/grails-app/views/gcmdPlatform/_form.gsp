<%@ page import="meta.GcmdPlatform" %>



<div class="fieldcontain ${hasErrors(bean: gcmdPlatformInstance, field: 'keyword', 'error')} required">
	<label for="keyword">
		<g:message code="gcmdPlatform.keyword.label" default="Keyword" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="keyword" required="" value="${gcmdPlatformInstance?.keyword}"/>
</div>

