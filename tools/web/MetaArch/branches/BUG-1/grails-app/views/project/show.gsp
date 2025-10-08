<%@ page import="meta.Project" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'project.label', default: 'Project')}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
	</head>
	<body>
		<a href="#show-project" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="show-project" class="content scaffold-show" role="main">
			<h1><g:message code="default.show.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			
			<!-- Display Project Members -->
			<div class="project-permitted">
				<h4>List of Project Members</h4>
				<g:listProjectEditors project="${projectInstance?.id}"></g:listProjectEditors>
				<g:ifInternalContact domain="project" id="${projectInstance?.id}">
					<br />
					<g:form>
						<fieldset>
							<legend>Member:</legend>
							<g:hiddenField name="id" value="${projectInstance?.id}" />
							<g:hiddenField name="cmd" value="add" />
							<g:selectMemberType project="${projectInstance?.id}" />
							<br />
							<g:textField id="memberSearchInput" name="username" onchange="" />
							<g:render template="/project/members" />
							<g:actionSubmit class="modifyGroup" action="modifyGroup" value="Add" />
						</fieldset>
					</g:form>
				</g:ifInternalContact>
<%--			</div>--%>
			<!-- End Display Project Members -->
			
			<!-- Display Required Fields -->
			<g:ifInternalContact domain="project" id="${projectInstance?.id}">
<%--			<div class="project-permitted">--%>
				<h4>Required Data Set Fields</h4>
				<g:render template="/requirement/reqs" />
			</g:ifInternalContact>
			</div>
			<!-- End Display Required Fields -->
			
			<g:if test="${projectInstance?.logo}">
			<!-- Display Project Logo -->
			<img class="project-logo" src="${projectInstance?.logo}" />
			<!-- End Display Project Logo -->
			</g:if>
			
			<ol class="property-list project">
			
				<g:if test="${projectInstance?.name}">
				<li class="fieldcontain">
					<span id="name-label" class="property-label">
						<g:message code="project.name.label" default="Name" />
					</span>
					
						<span class="property-value" aria-labelledby="name-label"><g:fieldValue bean="${projectInstance}" field="name"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.fullName}">
				<li class="fieldcontain">
					<span id="fullName-label" class="property-label">
						<g:message code="project.fullName.label" default="Full Name" />
					</span>
					
						<span class="property-value" aria-labelledby="fullName-label"><g:fieldValue bean="${projectInstance}" field="fullName"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.beginDate}">
				<li class="fieldcontain">
					<span id="beginDate-label" class="property-label"><g:message code="project.beginDate.label" default="Begin Date" /></span>
					
						<span class="property-value" aria-labelledby="beginDate-label"><g:formatDate date="${projectInstance?.beginDate}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.endDate}">
				<li class="fieldcontain">
					<span id="endDate-label" class="property-label"><g:message code="project.endDate.label" default="End Date" /></span>
					
						<span class="property-value" aria-labelledby="endDate-label"><g:formatDate date="${projectInstance?.endDate}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.fundingAgency}">
				<li class="fieldcontain">
					<span id="fundingAgency-label" class="property-label">
						<g:message code="project.fundingAgency.label" default="Funding Agency" />
					</span>
					
						<span class="property-value" aria-labelledby="fundingAgency-label"><g:link controller="agency" action="show" id="${projectInstance?.fundingAgency?.id}">${projectInstance?.fundingAgency?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.awardNumber}">
				<li class="fieldcontain">
					<span id="awardNumber-label" class="property-label"><g:message code="project.awardNumber.label" default="Award Number" /></span>
					
						<span class="property-value" aria-labelledby="awardNumber-label"><g:fieldValue bean="${projectInstance}" field="awardNumber"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.awardAmount}">
				<li class="fieldcontain">
					<span id="awardAmount-label" class="property-label"><g:message code="project.awardAmount.label" default="Award Amount" /></span>
					
						<span class="property-value" aria-labelledby="awardAmount-label"><g:fieldValue bean="${projectInstance}" field="awardAmount"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.summary}">
				<li class="fieldcontain">
					<span id="summary-label" class="property-label">
						<g:message code="project.summary.label" default="Summary" />
					</span>
						<span class="property-value" aria-labelledby="summary-label">${projectInstance?.summary}</span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.minLat}">
				<li class="fieldcontain">
					<span id="minLat-label" class="property-label"><g:message code="project.minLat.label" default="Minimum (South) Latitude" /></span>
					
						<span class="property-value" aria-labelledby="minLat-label"><g:fieldValue bean="${projectInstance}" field="minLat"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.maxLat}">
				<li class="fieldcontain">
					<span id="maxLat-label" class="property-label"><g:message code="project.maxLat.label" default="Maximum (North) Latitude" /></span>
					
						<span class="property-value" aria-labelledby="maxLat-label"><g:fieldValue bean="${projectInstance}" field="maxLat"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.minLon}">
				<li class="fieldcontain">
					<span id="minLon-label" class="property-label"><g:message code="project.minLon.label" default="Minimum (West) Longitude" /></span>
					
						<span class="property-value" aria-labelledby="minLon-label"><g:fieldValue bean="${projectInstance}" field="minLon"/></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.maxLon}">
				<li class="fieldcontain">
					<span id="maxLon-label" class="property-label"><g:message code="project.maxLon.label" default="Maximum (East) Longitude" /></span>
					
						<span class="property-value" aria-labelledby="maxLon-label"><g:fieldValue bean="${projectInstance}" field="maxLon"/></span>
					
				</li>
				</g:if>
				
				<g:if test="${projectInstance?.docGuideUrl}">
				<li class="fieldcontain">
					<span id="docGuideUrl-label" class="property-label"><g:message code="project.docGuideUrl.label" default="Project Documentation Guidelines" /></span>
					<span class="property-value" aria-labelledby="docGuideUrl-label"><a target="_blank" href="${projectInstance?.docGuideUrl}"><g:fieldValue bean="${projectInstance}" field="docGuideUrl"/></a></span>
				</li>
				</g:if>

				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP,ROLE_DMG">			
				<g:if test="${projectInstance?.cssUrl}">
				<li class="fieldcontain">
					<span id="cssUrl-label" class="property-label"><g:message code="project.cssUrl.label" default="Project Style" /></span>
					<br />
					<pre style='white-space:pre-wrap;padding:5px;background:#f9f9f9;border:1px solid #ccc;'><g:fieldValue bean="${projectInstance}" field="cssUrl"/></pre>
				</li>
				</g:if>
				</sec:ifAnyGranted>
			
				<g:if test="${projectInstance?.additionalMetadata}">
				<li class="fieldcontain">
					<span id="additionalMetadata-label" class="property-label"><g:message code="project.additionalMetadata.label" default="Additional Metadata" /></span>
					
						<span class="property-value" aria-labelledby="additionalMetadata-label"><g:link controller="xmlTemplate" action="show" id="${projectInstance?.additionalMetadata?.id}">${projectInstance?.additionalMetadata?.encodeAsHTML()}</g:link></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.dateCreated}">
				<li class="fieldcontain">
					<span id="dateCreated-label" class="property-label"><g:message code="project.dateCreated.label" default="Date Created" /></span>
					
						<span class="property-value" aria-labelledby="dateCreated-label"><g:formatDate date="${projectInstance?.dateCreated}" /></span>
					
				</li>
				</g:if>
			
				<g:if test="${projectInstance?.lastUpdated}">
				<li class="fieldcontain">
					<span id="lastUpdated-label" class="property-label"><g:message code="project.lastUpdated.label" default="Last Updated" /></span>
					
						<span class="property-value" aria-labelledby="lastUpdated-label"><g:formatDate date="${projectInstance?.lastUpdated}" /></span>
					
				</li>
				</g:if>
			
			</ol>
			
			<div class="ds-list">
				<h4>List of Recently Added Datasets</h4>
				<g:listDatasets project="${projectInstance?.id}"></g:listDatasets>
			</div>
			
			<g:ifPermitted domain="project" id="${projectInstance?.id}">
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${projectInstance?.id}" />
					<g:link class="edit" action="edit" id="${projectInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
					</sec:ifAnyGranted>
				</fieldset>
			</g:form>
			</g:ifPermitted>
		</div>
	</body>
</html>
