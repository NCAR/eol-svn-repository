<%@ page import="meta.DatasetPlatform" %>



<div class="fieldcontain ${hasErrors(bean: datasetPlatformInstance, field: 'dataset', 'error')} required">
	<label for="dataset">
		<g:message code="datasetPlatform.dataset.label" default="Dataset" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="dataset" name="dataset.id" from="${meta.Dataset.list()}" optionKey="id" required="" value="${datasetPlatformInstance?.dataset?.id}" class="many-to-one"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetPlatformInstance, field: 'platform', 'error')} required">
	<label for="platform">
		<g:message code="datasetPlatform.platform.label" default="Platform" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="platform" required="" value="${fieldValue(bean: datasetPlatformInstance, field: 'platform')}"/>
</div>

