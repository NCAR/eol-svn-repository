<%@ page import="meta.Dataset" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'dataset.label', default: 'Dataset')}" />
		<g:set var="dsProject" value="${datasetInstance?.project}" />
		<g:set var="miscMetadata" value="${dsProject?.additionalMetadata}" />
		<title><g:message code="default.edit.label" args="[entityName]" /></title>
		
	    <script type="text/javascript">
		    function updateProjectMetadata() {
		    	if ( document.getElementById('add-project-metadata') != null && document.getElementById('projectMetadata') != null ) {
			    	//var newMetadata = '<div id="add-project-metadata"> 	' + $('#add-project-metadata').html() + ' </div> <!-- end of project-specific metadata --> ';
			    	$('.dyn-field').each(function() {
				    	$(this).attr('value', $(this).attr('value'));
				    });
			    	
			    	$('#projectMetadata').attr('value', $('#add-project-metadata').html());
		    	}
		    }
	    </script>
	</head>
	<body>
		<a href="#edit-dataset" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="edit-dataset" class="content scaffold-edit" role="main">
			<h1><g:message code="default.edit.label" args="[entityName]" /></h1>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<g:hasErrors bean="${datasetInstance}">
			<ul class="errors" role="alert">
				<g:eachError bean="${datasetInstance}" var="error">
				<li <g:if test="${error in org.springframework.validation.FieldError}">data-field-id="${error.field}"</g:if>><g:message error="${error}"/></li>
				</g:eachError>
			</ul>
			</g:hasErrors>
			<g:form method="post" enctype="multipart/form-data" onsubmit="validateXlinks(); return updateProjectMetadata()">
				<g:hiddenField name="id" value="${datasetInstance?.id}" />
				<g:hiddenField name="version" value="${datasetInstance?.version}" />
				<fieldset class="form">
					<g:render template="form"/>
					<g:if test="${datasetInstance?.projectMetadata != null}">
						<g:set var="prjMeta">
							${datasetInstance.projectMetadata}
						</g:set>
					    <br /><br />
						<label id="shortcutMetadata" class="section-label">Additional Metadata for ${dsProject?.name}</label>
						<g:hiddenField name="projectMetadata" value="${prjMeta}" />
						<g:xmlForm xmlText="${datasetInstance.projectMetadata}" />
						<br />
					</g:if>
					<g:elseif test="${miscMetadata != null}">
						<g:set var="prjMeta"><g:xmlForm xmlFile="${miscMetadata}" /></g:set>
					    <br /><br />
					    <g:set var="miscMetadataContent"><g:xmlContent xmlFile="${miscMetadata}" /></g:set>
						<label id="shortcutMetadata" class="section-label">Additional Metadata for ${dsProject?.name}</label>
						<g:hiddenField name="projectMetadata" value="${miscMetadataContent}" />
						<g:xmlForm xmlText="${miscMetadataContent}" />
						<br />
					</g:elseif>
				</fieldset>
				<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
				<fieldset class="buttons">
					<g:actionSubmit class="save" action="update" before="updateProjectMetadata();" value="${message(code: 'dataset.button.update.label', default: 'Save Changes')}" />
					<g:actionSubmit class="delete" action="delete" value="${message(code: 'default.button.delete.label', default: 'Delete')}" formnovalidate="" onclick="return confirm('${message(code: 'default.button.delete.confirm.message', default: 'Are you sure?')}');" />
					<g:link class="show" action="show" id="${datasetInstance?.id}">Go Back</g:link>
				</fieldset>
				</g:ifPermitted>
			</g:form>
			<!-- Render the file/xlink templates as hidden so they can be cloned -->
			<g:render template="/xlink/xlink" model="['xlink':null,'i':'_clone', 'hidden':true]" />
			<g:render template="/file/file" model="['xlink':null,'i':'_clone', 'hidden':true]" />
			<!-- Render the file/xlink templates as hidden so they can be cloned -->
		</div>
	</body>
</html>
