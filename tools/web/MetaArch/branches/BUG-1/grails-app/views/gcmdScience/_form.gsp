<%@ page import="meta.GcmdScience" %>



<div class="fieldcontain ${hasErrors(bean: gcmdScienceInstance, field: 'keyword', 'error')} required">
	<label for="keyword">
		<g:message code="gcmdScience.keyword.label" default="Keyword" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="keyword" required="" value="${gcmdScienceInstance?.keyword}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: gcmdScienceInstance, field: 'parent', 'error')} ">
	<label for="parent">
		<g:message code="gcmdScience.parent.label" default="Parent" />
		
	</label>
	<g:select id="parent" name="parent.id" from="${meta.GcmdScience.list()}" optionKey="id" value="${gcmdScienceInstance?.parent?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

