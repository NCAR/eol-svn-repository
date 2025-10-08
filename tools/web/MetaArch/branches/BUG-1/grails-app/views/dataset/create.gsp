<%@ page import="meta.Dataset" %>
<!doctype html>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'dataset.label', default: 'Dataset')}" />
		<g:set var="dsProject" value="${datasetInstance?.project}" />
		<g:set var="miscMetadata" value="${dsProject?.additionalMetadata}" />
		<title><g:message code="default.create.label" args="[entityName]" /></title>
		<ckeditor:resources/>
		
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

		    function preSubmission() {
			    if ( verifyRequired() ) {
			    	validateXlinks();
			    	return updateProjectMetadata();
				} else {
					// Please fill out the required fields.
					return false;
				}
			}
	    </script>
	</head>
	<body>
		<a href="#create-dataset" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav" />
		</div>
		<div id="create-dataset" class="content scaffold-create" role="main">
			<h1><g:message code="default.create.label" args="[entityName]" /> for ${dsProject}</h1>
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
			<g:render template="/author/new" />
			<g:form method="post" enctype="multipart/form-data" onsubmit="return preSubmission()">
				<fieldset class="form">
					<g:render template="form"/>
					<g:if test="${datasetInstance?.projectMetadata}">
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
<%--					    <g:set var="prjMeta"><g:xmlForm xmlFile="${miscMetadata}" /></g:set>--%>
					    <br /><br />
					    <g:set var="miscMetadataContent"><g:xmlContent xmlFile="${miscMetadata}" /></g:set>
						<label class="section-label">Additional Metadata for ${dsProject?.name}</label>
						<g:hiddenField name="projectMetadata" value="${miscMetadataContent}" />
						<g:xmlForm xmlText="${miscMetadataContent}" />
						<br />
					</g:elseif>
				</fieldset>
				<fieldset class="buttons">
					<g:actionSubmit class="save" action="save" before="updateProjectMetadata();" value="${message(code: 'dataset.button.create.label', default: 'Save & Review')}" />
<%--					<g:submitButton name="create" class="save" before="javascript:updateProjectMetadata();" value="${message(code: 'dataset.button.create.label', default: 'Save & Review')}" />--%>
				</fieldset>
			</g:form>
			<!-- Render the file/xlink/author templates as hidden so they can be cloned -->
			<g:render template="/xlink/xlink" model="['xlink':null,'i':'_clone','hidden':true]" />
			<g:render template="/file/file" model="['file':null,'i':'_clone','hidden':true]" />
			<g:render template="/author/author" model="['author':null,'i':'_clone','hidden':true]" />
			<!-- Render the file/xlink/author templates as hidden so they can be cloned -->
		</div>
	</body>
</html>
