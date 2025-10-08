<%@ page import="meta.Dataset" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'dataset.label', default: 'Dataset')}" />
		<g:set var="dsProject" value="${datasetInstance?.project}" />
		<g:set var="miscMetadata" value="${dsProject?.additionalMetadata}" />
		<title><g:message code="default.show.label" args="[entityName]" /></title>
		
		<style type="text/css">
			 .large-label { style="font-size: 20px; width: 100%;" }
		</style>
		
		<g:if test="${flash.download}">
			<g:set var="download" value="${flash.download}" />
			${g.render(template:'/dataset/docDownload',model:['tFileId':download])}
		</g:if>
	</head>
	<body>
		<a href="#show-dataset" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav" />
		</div>
		
		<div class="sub-nav nav" role="navigation">
			<ul>
				<g:if test="${datasetInstance?.isSubmitted == false}">
					<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
						<li>
							<meta:tooltip domain="dataset" prop="isSubmitted" />
							<g:link action="addToArchive" id="${datasetInstance?.id}">Submit to Archive</g:link>
						</li>
					</g:ifPermitted>
				</g:if>
				<g:else>
					<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
						<li>
							<meta:tooltip domain="dataset" prop="isSubmitted" />
							<span style="display: inline-block; color: #666; padding: 0.25em 0.7em; font-style: italic;">Already Submitted</span>
						</li>
					</g:ifPermitted>
				</g:else>
				<li>
					<g:link class="clone" action="template" id="${datasetInstance?.id}">Copy this Data Set</g:link>
				</li>
				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP,ROLE_DMG">
				<li style="float: right;">
					<g:link action="allMetadata" id="${datasetInstance?.id}">Switch to Formatted View</g:link>
				</li>
				</sec:ifAnyGranted>
				<li style="float: right;">
					<g:link action="docWizard" id="${datasetInstance?.id}" title="Generate Documentation from Metadata">Documentation Creation Wizard</g:link>
				</li>
			</ul>
		</div>
		
		<div id="show-dataset" class="content scaffold-show" role="main">
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<g:if test="${flash.error}">
			<div class="errors" role="alert">${flash.error}</div>
			</g:if>
			
			<g:if test="${datasetInstance?.title}">
				<h1>
					<span aria-labelledby="title-label"><g:fieldValue bean="${datasetInstance}" field="title"/></span>
					<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
					<span class="property-label" style="float: right; font-style: italic; font-size: 16px;">created by <g:link controller="user" action="show" id="${datasetInstance?.owner?.id}">${datasetInstance?.owner?.realname?.encodeAsHTML()}</g:link></span>
					<br />
					</sec:ifAnyGranted>
				</h1>
			</g:if>
			
			
			
			<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
				<span id="ds-opts" class="nav" style="position: relative; top: -5px;">
					<g:link class="dataSave" action="editData" id="${datasetInstance?.id}">Edit Data / Documentation File List</g:link>
					<a class="dataSave" href="${createLink(action: 'edit', params: [id: datasetInstance?.id])}#xlinkListdiv">Edit Links</a>
					<g:if test="${datasetInstance?.projectMetadata}">
						<a class="dataSave" href="${createLink(action: 'edit', params: [id: datasetInstance?.id])}#shortcutMetadata">Edit Additional Metadata</a><br />
					</g:if>
				</span>
				<span style="clear: both;"></span>
				<br />
			</g:ifPermitted>
			
			
			
			
			<div class="property-list dataset">
			
			<g:if test="${datasetInstance?.owner && datasetInstance?.pointOfContact}">
				<div class="fieldcontain" style="margin-top: 0; padding-bottom: 0;">
					<span class="property-label" aria-labelledby="pointOfContact-label">Point of Contact: </span>
					<span class="property-value">
						<g:link controller="user" action="show" id="${datasetInstance?.pointOfContact?.id}">${datasetInstance?.pointOfContact?.realname?.encodeAsHTML()}</g:link>
						
						<g:if test="${datasetInstance?.fundingAgency && datasetInstance?.awardNumber}">
						<span style="float:right;" aria-labelledby="funding-label">
							<span style="color: #666666;">Funded by: &nbsp;</span>
							${datasetInstance?.fundingAgency?.encodeAsHTML()}&nbsp;<g:fieldValue bean="${datasetInstance}" field="awardNumber"/>
							<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
							<g:if test="${datasetInstance?.awardAmount}">&nbsp;(&#36;<g:fieldValue bean="${datasetInstance}" field="awardAmount"/>)</g:if>
							</sec:ifAnyGranted>
						</span>
						<span style="clear: both;"></span>
						</g:if>
					</span>
				</div>
			</g:if>
			
				<g:if test="${datasetInstance?.summary}">
				<div class="fieldcontain">
					<span id="summary-label" class="large-label property-label"><g:message code="dataset.summary.label" default="Summary" /></span>
					<br />
					<pre style='white-space: pre-wrap; font-family: "HelveticaNeue-Light","Helvetica Neue Light","Helvetica Neue",Helvetica,Arial,"Lucida Grande",sans-serif;'>${datasetInstance?.summary}</pre>
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.datasetVersion}">
				<div class="fieldcontain">
					<span id="datasetVersion-label" class="property-label"><g:message code="dataset.datasetVersion.label" default="Dataset Version" /></span>
					
						<span class="property-value" aria-labelledby="datasetVersion-label"><g:fieldValue bean="${datasetInstance}" field="datasetVersion"/></span>
					
				</div>
				</g:if>
				
				<g:if test="${datasetInstance?.hasAnyAuthors()}">
				<div class="fieldcontain">
					<span id="authors-label" class="property-label"><g:message code="dataset.authors.label" default="Authors" /></span>
					<span class="property-value" aria-labelledby="authors-label">
						<ol>
						<g:each in="${datasetInstance?.authorList().sort{it.sortKey}}" var="author" status="i">
							<li>${author.author}</li>
						</g:each>
						</ol>
					</span>
				</div>
				</g:if>
			
<%--				<g:if test="${datasetInstance?.owner}">--%>
<%--				<li class="fieldcontain">--%>
<%--					<span id="owner-label" class="property-label"><g:message code="dataset.owner.label" default="Owner" /></span>--%>
<%--					--%>
<%--						<span class="property-value" aria-labelledby="owner-label"><g:link controller="user" action="show" id="${datasetInstance?.owner?.id}">${datasetInstance?.owner?.encodeAsHTML()}</g:link></span>--%>
<%--					--%>
<%--				</li>--%>
<%--				</g:if>--%>
			
				<br />
				<label class="section-label large-label">Additional Information</label>
				<g:if test="${datasetInstance?.project}">
				<div class="fieldcontain">
					<span id="project-label" class="property-label"><g:message code="dataset.project.label" default="Project" /></span>
					<span class="property-value" aria-labelledby="project-label"><g:link controller="project" action="show" id="${datasetInstance?.project?.id}">${datasetInstance?.project?.encodeAsHTML()}</g:link></span>
				</div>
				</g:if>
				
				<g:if test="${datasetInstance?.frequency}">
				<div class="fieldcontain">
					<span id="frequency-label" class="property-label"><g:message code="dataset.frequency.label" default="Observational Frequency" /></span>
					<span class="property-value" aria-labelledby="frequency-label">${meta.Frequency.get(datasetInstance?.frequency).encodeAsHTML()}</span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.spatialType}">
				<div class="fieldcontain">
					<span id="spatialType-label" class="property-label"><g:message code="dataset.spatialType.label" default="Spatial Type" /></span>
					<span class="property-value" aria-labelledby="spatialType-label"><g:fieldValue bean="${datasetInstance}" field="spatialType"/></span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.datasetCategories}">
				<div class="fieldcontain">
					<span id="datasetCategories-label" class="property-label"><g:message code="dataset.datasetCategories.label" default="Categories" /></span>
					<span class="property-value" aria-labelledby="datasetCategories-label">
						<g:each in="${datasetInstance?.categories()}" var="category" status="i">
						${meta.Category.get(category).encodeAsHTML()}<g:if test="${i != datasetInstance?.categories().size() - 1}">,&nbsp;</g:if>
						</g:each>
					</span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.datasetPlatforms}">
				<div class="fieldcontain">
					<span id="datasetPlatforms-label" class="property-label"><g:message code="dataset.datasetPlatforms.label" default="Platforms" /></span>
					<span class="property-value" aria-labelledby="datasetPlatforms-label">
						<g:each in="${datasetInstance?.platforms()}" var="platform" status="i">
						${meta.Platform.get(platform).encodeAsHTML()}<g:if test="${i != datasetInstance?.platforms().size() - 1}">,&nbsp;</g:if>
						</g:each>
					</span>
				</div>
				</g:if>
				
				<g:if test="${datasetInstance?.scienceKeyword}">
				<div class="side-by-side fieldcontain">
					<span id="scienceKeyword-label" class="property-label"><g:message code="dataset.scienceKeyword.label" default="Science Keyword" /></span>
					<span class="property-value" aria-labelledby="scienceKeyword-label"><g:link controller="gcmdScience" action="show" id="${datasetInstance?.scienceKeyword?.id}">${datasetInstance?.scienceKeyword?.encodeAsHTML()}</g:link></span>
				</div>
				</g:if>			
				<g:if test="${datasetInstance?.locationKeyword}">
				<div class="side-by-side fieldcontain">
					<span id="locationKeyword-label" class="property-label"><g:message code="dataset.locationKeyword.label" default="Location Keyword" /></span>
					<span class="property-value" aria-labelledby="locationKeyword-label"><g:link controller="gcmdLocation" action="show" id="${datasetInstance?.locationKeyword?.id}">${datasetInstance?.locationKeyword?.encodeAsHTML()}</g:link></span>
				</div>
				</g:if>			
				<g:if test="${datasetInstance?.platformKeyword}">
				<div class="side-by-side fieldcontain">
					<span id="platformKeyword-label" class="property-label"><g:message code="dataset.platformKeyword.label" default="Platform Keyword" /></span>					
					<span class="property-value" aria-labelledby="platformKeyword-label"><g:link controller="gcmdPlatform" action="show" id="${datasetInstance?.platformKeyword?.id}">${datasetInstance?.platformKeyword?.encodeAsHTML()}</g:link></span>					
				</div>
				</g:if>			
				<g:if test="${datasetInstance?.instrumentKeyword}">
				<div class="side-by-side fieldcontain">
					<span id="instrumentKeyword-label" class="property-label"><g:message code="dataset.instrumentKeyword.label" default="Instrument Keyword" /></span>
					<span class="property-value" aria-labelledby="instrumentKeyword-label"><g:link controller="gcmdInstrument" action="show" id="${datasetInstance?.instrumentKeyword?.id}">${datasetInstance?.instrumentKeyword?.encodeAsHTML()}</g:link></span>
				</div>
				</g:if>
				<div style="clear:both;"></div>
			
				<g:if test="${datasetInstance?.topic}">
				<div class="fieldcontain">
					<span id="topic-label" class="property-label"><g:message code="dataset.topic.label" default="ISO Topic" /></span>
					
						<span class="property-value" aria-labelledby="topic-label"><g:link controller="isoTopic" action="show" id="${datasetInstance?.topic?.id}">${datasetInstance?.topic?.encodeAsHTML()}</g:link></span>
					
				</div>
				</g:if>
			
			
			
				<br />
				<label class="section-label large-label">Temporal Coverage</label>
				<g:if test="${datasetInstance?.beginDate}">
				<div class="fieldcontain dateRange">
					<span id="beginDate-label" class="property-label"><g:message code="dataset.beginDate.label" default="Begin Date" /></span>
					
						<span class="property-value" aria-labelledby="beginDate-label"><g:formatDate date="${datasetInstance?.beginDate}" /></span>
					
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.endDate}">
				<div class="fieldcontain dateRange">
					<span id="endDate-label" class="property-label"><g:message code="dataset.endDate.label" default="End Date" /></span>
					
						<span class="property-value" aria-labelledby="endDate-label"><g:formatDate date="${datasetInstance?.endDate}" /></span>
					
				</div>
				</g:if>
				<div style="clear:both;"></div>
			
			
			
				<br />
				<label class="section-label large-label">Spatial Coverage</label>
				<g:if test="${datasetInstance?.minLat}">
				<div class="fieldcontain">
					<span id="minLat-label" class="property-label"><g:message code="dataset.minLat.label" default="Minimum (South) Latitude" /></span>
					<span class="property-value" aria-labelledby="minLat-label"><g:fieldValue bean="${datasetInstance}" field="minLat"/></span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.maxLat}">
				<div class="fieldcontain">
					<span id="maxLat-label" class="property-label"><g:message code="dataset.maxLat.label" default="Maximum (North) Latitude" /></span>
					<span class="property-value" aria-labelledby="maxLat-label"><g:fieldValue bean="${datasetInstance}" field="maxLat"/></span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.minLon}">
				<div class="fieldcontain">
					<span id="minLon-label" class="property-label"><g:message code="dataset.minLon.label" default="Minimum (West) Longitude" /></span>
					<span class="property-value" aria-labelledby="minLon-label"><g:fieldValue bean="${datasetInstance}" field="minLon"/></span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.maxLon}">
				<div class="fieldcontain">
					<span id="maxLon-label" class="property-label"><g:message code="dataset.maxLon.label" default="Maximum (East) Longitude" /></span>
					<span class="property-value" aria-labelledby="maxLon-label"><g:fieldValue bean="${datasetInstance}" field="maxLon"/></span>
				</div>
				</g:if>
				<div style="clear:both;"></div>
				
			
				<g:if test="${datasetInstance?.horizontalResolution || datasetInstance?.verticalResolution}">
				<br />
				<label class="section-label large-label">Resolution</label>
				</g:if>
				<g:if test="${datasetInstance?.horizontalResolution}">
				<div class="fieldcontain">
					<span id="horizontalResolution-label" class="property-label"><g:message code="dataset.horizontalResolution.label" default="Horizontal Resolution" /></span>
					<span class="property-value" aria-labelledby="horizontalResolution-label"><g:link controller="horizontalResolution" action="show" id="${datasetInstance?.horizontalResolution?.id}">${datasetInstance?.horizontalResolution?.encodeAsHTML()}</g:link></span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.verticalResolution}">
				<div class="fieldcontain">
					<span id="verticalResolution-label" class="property-label"><g:message code="dataset.verticalResolution.label" default="Vertical Resolution" /></span>
					<span class="property-value" aria-labelledby="verticalResolution-label"><g:link controller="verticalResolution" action="show" id="${datasetInstance?.verticalResolution?.id}">${datasetInstance?.verticalResolution?.encodeAsHTML()}</g:link></span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.horizontalResolution || datasetInstance?.verticalResolution}">
				<div style="clear:both;"></div>
				</g:if>
			
				
				<br />
				<label class="section-label large-label">Status, Restrictions, Tags, etc.</label>
				<g:if test="${datasetInstance?.progress}">
				<div class="fieldcontain">
					<span id="progress-label" class="property-label"><g:message code="dataset.progress.label" default="Progress" /></span>
					
						<span class="property-value" aria-labelledby="progress-label"><g:fieldValue bean="${datasetInstance}" field="progress"/></span>
					
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.accessRestrictions}">
				<div class="fieldcontain">
					<span id="accessRestrictions-label" class="property-label"><g:message code="dataset.accessRestrictions.label" default="Access Restrictions" /></span>
					
						<span class="property-value" aria-labelledby="accessRestrictions-label"><g:fieldValue bean="${datasetInstance}" field="accessRestrictions"/></span>
					
				</div>
				</g:if>
				
			
				<g:if test="${datasetInstance?.dataTags}">
				<div class="fieldcontain">
					<span id="dataTags-label" class="property-label"><g:message code="dataset.dataTags.label" default="Data Tags" /></span>
					
						<span class="property-value" aria-labelledby="dataTags-label"><g:fieldValue bean="${datasetInstance}" field="dataTags"/></span>
					
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.language}">
				<div class="fieldcontain">
					<span id="language-label" class="property-label"><g:message code="dataset.language.label" default="Language" /></span>
					
						<span class="property-value" aria-labelledby="language-label"><g:fieldValue bean="${datasetInstance}" field="language"/></span>
					
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.comments}">
				<div class="fieldcontain">
					<span id="comments-label" class="property-label"><g:message code="dataset.comments.label" default="Comments" /></span>
					
						<span class="property-value" aria-labelledby="comments-label">
							<pre style='white-space: pre-wrap; font-family: "HelveticaNeue-Light","Helvetica Neue Light","Helvetica Neue",Helvetica,Arial,"Lucida Grande",sans-serif;'><g:fieldValue bean="${datasetInstance}" field="comments"/></pre>
						</span>
					
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.metadataVersion}">
				<div class="fieldcontain">
					<span id="metadataVersion-label" class="property-label"><g:message code="dataset.metadataVersion.label" default="Metadata Version" /></span>
					
						<span class="property-value" aria-labelledby="metadataVersion-label"><g:fieldValue bean="${datasetInstance}" field="metadataVersion"/></span>
					
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.versionNumber}">
				<div class="fieldcontain">
					<span id="versionNumber-label" class="property-label"><g:message code="dataset.versionNumber.label" default="Version Number" /></span>
					
						<span class="property-value" aria-labelledby="versionNumber-label"><g:fieldValue bean="${datasetInstance}" field="versionNumber"/></span>
					
				</div>
				</g:if>
				<div style="clear:both;"></div>
			
				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">
				<g:if test="${datasetInstance?.dateCreated}">
				<div class="fieldcontain">
					<span id="dateCreated-label" class="property-label"><g:message code="dataset.dateCreated.label" default="Date Created" /></span>
					
						<span class="property-value" aria-labelledby="dateCreated-label"><g:formatDate date="${datasetInstance?.dateCreated}" /></span>
					
				</div>
				</g:if>
			
				<g:if test="${datasetInstance?.lastUpdated}">
				<div class="fieldcontain">
					<span id="lastUpdated-label" class="property-label"><g:message code="dataset.lastUpdated.label" default="Last Updated" /></span>
					
						<span class="property-value" aria-labelledby="lastUpdated-label"><g:formatDate date="${datasetInstance?.lastUpdated}" /></span>
					
				</div>
				</g:if>
				</sec:ifAnyGranted>
			
			
				<g:if test="${datasetInstance?.xlinks != null || datasetInstance?.files != null}">
				<br />
				<label class="section-label large-label">Data / Documentation Files &amp; Links</label>
				<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
					<span id="ds-opts-2" class="nav" style="position: relative; top: 5px;">
						<g:link class="dataSave" action="editData" id="${datasetInstance?.id}">Edit Data / Documentation File List</g:link>
						<a class="dataSave" href="${createLink(action: 'edit', params: [id: datasetInstance?.id])}#xlinkListdiv">Edit Links</a>
					</span>
				</g:ifPermitted>
				</g:if>
				<g:if test="${datasetInstance?.files}">
				<div class="fieldcontain">
					<span id="files-label" class="property-label"><g:message code="dataset.files.label" default="Data / Documentation Files" /></span>
					<span class="property-value" aria-labelledby="files-label">
						<ul>
						<g:each in="${datasetInstance?.files}" var="file" status="i">
							<li title="${file.name}">${file.fileType}: <sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP">${file.directory}<g:if test="${!(file.directory =~ /.*\/$/)}">/</g:if></sec:ifAnyGranted>${file.name} ([${file.format}], ${file.size} kB)</li>
						</g:each>
						</ul>
					</span>
				</div>
				</g:if>
				<g:if test="${datasetInstance?.xlinks}">
				<div class="fieldcontain">
					<span id="xlinks-label" class="property-label"><g:message code="dataset.xlinks.label" default="Links" /></span>
					<span class="property-value" aria-labelledby="xlinks-label">
						<ul>
						<g:each in="${datasetInstance?.xlinks}" var="xlink" status="i">
							<li>${xlink.type.name()}: <a target="_blank" href="${xlink.href}"><g:if test="${xlink.title != ''}">${xlink.title}</g:if><g:else>${xlink.href}</g:else></a></li>
						</g:each>
						</ul>
					</span>
				</div>
				</g:if>
				
				
				<g:if test="${datasetInstance?.projectMetadata}">
				    <br /><br />
					<label class="section-label">Additional Metadata for ${datasetInstance?.project?.name}</label>
					<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
						<span id="ds-opts-3" class="nav" style="position: relative; top: 5px;">
							<a class="dataSave" href="${createLink(action: 'edit', params: [id: datasetInstance?.id])}#shortcutMetadata">Edit Additional Metadata</a><br />
						</span>
					</g:ifPermitted>
					<g:hiddenField name="projectMetadata" value="${datasetInstance?.projectMetadata}" />
					<g:xmlShow xmlText="${datasetInstance.projectMetadata}" />
					<br />
				</g:if>
<%--				<g:elseif test="${miscMetadata}">--%>
<%--				    <br /><br />--%>
<%--					<label class="section-label">Additional Metadata for ${datasetInstance?.project?.name}</label>--%>
<%--					<g:hiddenField name="projectMetadata" value="${datasetInstance?.projectMetadata}" />--%>
<%--					<g:xmlShow xmlText="${miscMetadata}" />--%>
<%--					<br />--%>
<%--				</g:elseif>		--%>
			
			</div>		
			
			
			<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
			<g:form>
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${datasetInstance?.id}" />
					<g:if test="${datasetInstance?.isSubmitted == false}">
						<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
<%--							<meta:tooltip domain="dataset" prop="isSubmitted" />--%>
							<g:link action="addToArchive" id="${datasetInstance?.id}">Submit to Archive</g:link>
						</g:ifPermitted>
					</g:if>
					<g:else>
						<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
							<meta:tooltip domain="dataset" prop="isSubmitted" />
							<span style="display: inline-block; color: #666; padding: 0.25em 0.7em; font-style: italic;">Already Submitted</span>
						</g:ifPermitted>
					</g:else>
					<g:link class="edit" action="edit" id="${datasetInstance?.id}"><g:message code="default.button.edit.label" default="Edit" /></g:link>
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
				</fieldset>
			</g:form>
			</g:ifPermitted>
		</div>
	</body>
</html>
