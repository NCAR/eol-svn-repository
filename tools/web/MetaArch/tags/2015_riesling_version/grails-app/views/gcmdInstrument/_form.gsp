<%@ page import="meta.GcmdInstrument" %>



<div class="fieldcontain ${hasErrors(bean: gcmdInstrumentInstance, field: 'keyword', 'error')} required">
	<label for="keyword">
		<g:message code="gcmdInstrument.keyword.label" default="Keyword" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="keyword" required="" value="${gcmdInstrumentInstance?.keyword}"/>
</div>

