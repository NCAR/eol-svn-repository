<%@ page import="meta.DatasetCategory" %>



<div class="fieldcontain ${hasErrors(bean: datasetCategoryInstance, field: 'category', 'error')} required">
	<label for="category">
		<g:message code="datasetCategory.category.label" default="Category" />
		<span class="required-indicator">*</span>
	</label>
	<g:field type="number" name="category" required="" value="${fieldValue(bean: datasetCategoryInstance, field: 'category')}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: datasetCategoryInstance, field: 'dataset', 'error')} required">
	<label for="dataset">
		<g:message code="datasetCategory.dataset.label" default="Dataset" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="dataset" name="dataset.id" from="${meta.Dataset.list()}" optionKey="id" required="" value="${datasetCategoryInstance?.dataset?.id}" class="many-to-one"/>
</div>

