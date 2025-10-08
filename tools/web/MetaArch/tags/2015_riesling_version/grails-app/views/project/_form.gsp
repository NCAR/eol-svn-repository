<%@ page import="meta.Project" %>



<div class="title fieldcontain ${hasErrors(bean: projectInstance, field: 'name', 'error')} required">
	<label for="name">
		<g:message code="project.name.label" default="Name" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="name" maxlength="255" required="" value="${projectInstance?.name}"/>
</div>
<div style="clear: both;"></div>

<div class="title fieldcontain ${hasErrors(bean: projectInstance, field: 'fullName', 'error')} required">
	<label for="fullName">
		<g:message code="project.fullName.label" default="Full Name" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="fullName" maxlength="255" required="" value="${projectInstance?.fullName}"/>
</div>
<div style="clear: both;"></div>

<div class="fieldcontain ${hasErrors(bean: projectInstance, field: 'summary', 'error')} ">
	<label for="summary">
		<g:message code="project.summary.label" default="Summary" />
		
	</label>
	<g:textArea name="summary" cols="40" rows="5" maxlength="65535" value="${projectInstance?.summary}"/>
</div>


<br /><br />
<label class="section-label">Funding Information</label>
<div class="fieldcontain ${hasErrors(bean: projectInstance, field: 'fundingAgency', 'error')} required">
	<label for="fundingAgency">
		<g:message code="project.fundingAgency.label" default="Funding Agency" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="fundingAgency" name="fundingAgency.id" from="${meta.Agency.list()}" optionKey="id" required="" value="${projectInstance?.fundingAgency?.id}" class="many-to-one"/>
</div>

<div class="fieldcontain ${hasErrors(bean: projectInstance, field: 'awardNumber', 'error')} required">
	<label for="awardNumber">
		<g:message code="project.awardNumber.label" default="Award Number" />
		<span class="required-indicator">*</span>
	</label>
	<g:textField name="awardNumber" required="" value="${projectInstance?.awardNumber}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: projectInstance, field: 'awardAmount', 'error')} ">
	<label for="awardAmount">
		<g:message code="project.awardAmount.label" default="Award Amount" />
		
	</label>
	<g:field type="number" name="awardAmount" value="${fieldValue(bean: projectInstance, field: 'awardAmount')}"/>
</div>

<br /><br />
<label class="section-label">Date Range</label>
<div class="dateRange fieldcontain ${hasErrors(bean: projectInstance, field: 'beginDate', 'error')} ">
	<label for="beginDate">
		<g:message code="project.beginDate.label" default="Begin Date" />
		
	</label>
	<g:datePicker name="beginDate" precision="minute" value="${projectInstance?.beginDate}" default="none" noSelection="['': '']" />
</div>

<div class="dateRange fieldcontain ${hasErrors(bean: projectInstance, field: 'endDate', 'error')} ">
	<label for="endDate">
		<g:message code="project.endDate.label" default="End Date" />
		
	</label>
	<g:datePicker name="endDate" precision="minute" value="${projectInstance?.endDate}" default="none" noSelection="['': '']" />
</div>
<div style="clear: both;"></div>



<br /><br />
<label class="section-label">Bounding Coordinates</label>
<div class="coords fieldcontain ${hasErrors(bean: projectInstance, field: 'minLat', 'error')} ">
	<label for="minLat">
		<g:message code="project.minLat.label" default="Minimum (South) Latitude" />
		
	</label>
	<g:field type="number" name="minLat" min="-90" max="90" step="any" value="${fieldValue(bean: projectInstance, field: 'minLat')}"/>
</div>

<div class="coords fieldcontain ${hasErrors(bean: projectInstance, field: 'maxLat', 'error')} ">
	<label for="maxLat">
		<g:message code="project.maxLat.label" default="Maximum (North) Latitude" />
		
	</label>
	<g:field type="number" name="maxLat" min="-90" max="90" step="any" value="${fieldValue(bean: projectInstance, field: 'maxLat')}"/>
</div>

<div class="coords fieldcontain ${hasErrors(bean: projectInstance, field: 'minLon', 'error')} ">
	<label for="minLon">
		<g:message code="project.minLon.label" default="Minimum (West) Longitude" />
		
	</label>
	<g:field type="number" name="minLon" min="-180" max="180" step="any" value="${fieldValue(bean: projectInstance, field: 'minLon')}"/>
</div>

<div class="coords fieldcontain ${hasErrors(bean: projectInstance, field: 'maxLon', 'error')} ">
	<label for="maxLon">
		<g:message code="project.maxLon.label" default="Maximum (East) Longitude" />
		
	</label>
	<g:field type="number" name="maxLon" min="-180" max="180" step="any" value="${fieldValue(bean: projectInstance, field: 'maxLon')}"/>
</div>
<div style="clear: both;"></div>


<br /><br />
<label class="section-label">Look &amp; Feel</label>
<div class="title fieldcontain ${hasErrors(bean: projectInstance, field: 'cssUrl', 'error')} ">
	<label for="cssUrl">
		<g:message code="project.cssUrl.label" default="Project Style (CSS)" />
		
	</label>
	<g:textArea name="cssUrl" value="${projectInstance?.cssUrl}"/>
</div>
<div style="clear: both;"></div>

<div class="fieldcontain ${hasErrors(bean: projectInstance, field: 'additionalMetadata', 'error')} ">
	<label for="additionalMetadata">
		<g:message code="project.additionalMetadata.label" default="Additional Metadata" />
		
	</label>
	<g:select id="additionalMetadata" name="additionalMetadata.id" from="${meta.XmlTemplate.list()}" optionKey="id" value="${projectInstance?.additionalMetadata?.id}" class="many-to-one" noSelection="['null': '']"/>
</div>

