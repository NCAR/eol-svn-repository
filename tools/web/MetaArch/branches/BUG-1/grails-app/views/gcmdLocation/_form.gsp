<%@ page import="meta.GcmdLocation" %>



<div class="fieldcontain ${hasErrors(bean: gcmdLocationInstance, field: 'keyword', 'error')} required">
	<label for="keyword">
		<g:message code="gcmdLocation.keyword.label" default="Keyword" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="keyword" required="" value="${gcmdLocationInstance?.keyword}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: gcmdLocationInstance, field: 'parent', 'error')} ">
	<label for="parent">
		<g:message code="gcmdLocation.parent.label" default="Parent" />
		
	</label>
	<g:select id="parent" name="parent.id" from="${meta.GcmdLocation.list()}" optionKey="id" value="${gcmdLocationInstance?.parent?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

