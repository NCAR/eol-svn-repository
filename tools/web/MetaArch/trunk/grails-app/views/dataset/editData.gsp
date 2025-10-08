<%@ page contentType="text/html;charset=ISO-8859-1" %>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'Dataset.label', default: 'Dataset')}" />
		<title><g:message code="default.create.label" args="[entityName]" /> - Upload Files</title>
		
		<style type="text/css">
			.prj {
				margin: 10px;
				float: left;
				border: 1px solid #ccc;
			}
			.prjtop {
				background: #90BCD5;
				padding: 5px 10px;
			}
			.prjtop .name a { color: #333; text-decoration: none; font-weight: bold; }
			.prjbox {
				background: #e7e7e7;
				padding: 10px;
			}
		</style>
	</head>
<body>
	<a href="#edit-dataset-data" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
	<div class="nav" role="navigation">
			<g:render template="nav" />
		</div>
	
	<div id="edit-dataset-data" class="content scaffold-show" role="main">
		<g:if test="${datasetInstance?.title}">
			<h1>
				<span aria-labelledby="title-label"><g:fieldValue bean="${datasetInstance}" field="title"/>- Upload Files</span>
			</h1>
		</g:if>
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
		
<%--		<p class="tooltip" style="margin: 0em 1.5em;">--%>
<%--			Have you created documentation for this data set?--%>
<%--			&nbsp;--%>
<%--			If not, you can refer to the <a target="_blank" href="${createLink(uri: '/docFormatGuide')}">documentation and format guidelines</a> for assistance.--%>
<%--		</p>--%>
		
		<g:form controller="file" action="saveMultiple" onSubmit="validateFiles();" enctype="multipart/form-data">
			<fieldset class="form">
			<g:render template="/file/files" model="['datasetInstance':datasetInstance]" />
			</fieldset>
			<br />
			<fieldset class="buttons">
				<g:hiddenField name="dataset.id" value="${datasetInstance?.id}" />
				<g:submitButton class="dataSave" before="validateFiles();" name="saveMultiple" value="Save Changes" />
				<g:link class="show" action="show" id="${datasetInstance?.id}">Go Back</g:link>
			</fieldset>
		</g:form>
		<!-- Render the file template as hidden so they can be cloned -->
		<g:render template="/file/file" model="['xlink':null,'i':'_clone', 'hidden':true]" />
		<!-- Render the file template as hidden so they can be cloned -->
	</div>
	
</body>
</html>