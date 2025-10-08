<%@ page import="meta.DatasetAuthor" %>



<div class="fieldcontain ${hasErrors(bean: datasetAuthorInstance, field: 'author', 'error')} required">
	<label for="author">
		<g:message code="datasetAuthor.author.label" default="Author" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="author" name="author.id" from="${meta.Author.list()}" optionKey="id" required="" value="${datasetAuthorInstance?.author?.id}" class="many-to-one"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetAuthorInstance, field: 'dataset', 'error')} required">
	<label for="dataset">
		<g:message code="datasetAuthor.dataset.label" default="Dataset" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="dataset" name="dataset.id" from="${meta.Dataset.list()}" optionKey="id" required="" value="${datasetAuthorInstance?.dataset?.id}" class="many-to-one"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetAuthorInstance, field: 'sortKey', 'error')} required">
	<label for="sortKey">
		<g:message code="datasetAuthor.sortKey.label" default="Sort Key" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="sortKey" required="" value="${fieldValue(bean: datasetAuthorInstance, field: 'sortKey')}"/>
</div>

