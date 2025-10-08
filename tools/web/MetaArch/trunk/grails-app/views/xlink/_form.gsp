<%@ page import="meta.Xlink" %>



<div class="fieldcontain ${hasErrors(bean: xlinkInstance, field: 'index', 'error')} required">
	<label for="index">
		<g:message code="xlink.index.label" default="Index" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="index" min="0" required="" value="${fieldValue(bean: xlinkInstance, field: 'index')}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: xlinkInstance, field: 'title', 'error')} required">
	<label for="title">
		<g:message code="xlink.title.label" default="Title" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="title" maxlength="255" required="" value="${xlinkInstance?.title}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: xlinkInstance, field: 'type', 'error')} required">
	<label for="type">
		<g:message code="xlink.type.label" default="Type" />
		<span class="required-indicator">*</span>
	</label>
	<g:select name="type" from="${meta.XlinkType?.values()}" keys="${meta.XlinkType.values()*.name()}" required="" value="${xlinkInstance?.type?.name()}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: xlinkInstance, field: 'href', 'error')} ">
	<label for="href">
		<g:message code="xlink.href.label" default="Href" />
		
	</label>
	<g:textArea name="href" cols="40" rows="5" maxlength="4096" value="${xlinkInstance?.href}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: xlinkInstance, field: 'dataset', 'error')} required">
	<label for="dataset">
		<g:message code="xlink.dataset.label" default="Dataset" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="dataset" name="dataset.id" from="${meta.Dataset.list()}" optionKey="id" required="" value="${xlinkInstance?.dataset?.id}" class="many-to-one"/>
</div>

