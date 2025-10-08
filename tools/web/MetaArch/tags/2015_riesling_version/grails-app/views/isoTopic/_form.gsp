<%@ page import="meta.IsoTopic" %>



<div class="fieldcontain ${hasErrors(bean: isoTopicInstance, field: 'keyword', 'error')} required">
	<label for="keyword">
		<g:message code="isoTopic.keyword.label" default="Keyword" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="keyword" required="" value="${isoTopicInstance?.keyword}"/>
</div>

