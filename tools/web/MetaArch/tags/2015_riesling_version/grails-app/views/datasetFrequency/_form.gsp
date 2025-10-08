<%@ page import="meta.DatasetFrequency" %>



<div class="fieldcontain ${hasErrors(bean: datasetFrequencyInstance, field: 'dataset', 'error')} required">
	<label for="dataset">
		<g:message code="datasetFrequency.dataset.label" default="Dataset" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="dataset" name="dataset.id" from="${meta.Dataset.list()}" optionKey="id" required="" value="${datasetFrequencyInstance?.dataset?.id}" class="many-to-one"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetFrequencyInstance, field: 'frequency', 'error')} required">
	<label for="frequency">
		<g:message code="datasetFrequency.frequency.label" default="Frequency" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="frequency" required="" value="${fieldValue(bean: datasetFrequencyInstance, field: 'frequency')}"/>
</div>

