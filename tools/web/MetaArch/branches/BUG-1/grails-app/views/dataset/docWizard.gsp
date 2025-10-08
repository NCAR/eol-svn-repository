<%@ page contentType="text/html;charset=ISO-8859-1" %>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'Dataset.label', default: 'Dataset')}" />
		<title><g:message code="default.create.label" args="[entityName]" /> - Documentation Creation Wizard</title>
		<ckeditor:resources/>
		
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
			
			label, input { display:block; }
			input.text { margin-bottom:12px; width:95%; padding: .4em; }
			fieldset { padding:0; border:0; margin-top:25px; }
			h1 { font-size: 1.2em; margin: .6em 0; }
			div#users-contain { width: 350px; margin: 20px 0; }
			div#users-contain table { margin: 1em 0; border-collapse: collapse; width: 100%; }
			div#users-contain table td, div#users-contain table th { border: 1px solid #eee; padding: .6em 10px; text-align: left; }
			.ui-dialog .ui-state-error { padding: .3em; }
			.validateTips { font-style: italic; border: 1px solid transparent; padding-bottom: 0.3em; }
		</style>
		
		<script src="${resource(dir: 'js', file: 'jquery-ui-1.10.3.custom.js')}" type="text/javascript"></script>
		<link rel="stylesheet" href="${resource(dir: 'css/meta-theme', file: 'jquery-ui-1.10.3.custom.css')}" type="text/css" />
		<script src="${resource(dir: 'js', file: 'download-dialog.js')}" type="text/javascript"></script>
	</head>
	<body>
		<a href="#edit-dataset-data" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav" />
		</div>
		
		<div id="edit-dataset-data" class="content scaffold-show" role="main">
			<g:if test="${datasetInstance?.title}">
				<h1>
					<span aria-labelledby="title-label"><g:fieldValue bean="${datasetInstance}" field="title"/>- Documentation Creation Wizard</span>
				</h1>
			</g:if>
			<g:if test="${flash.message}">
			<div class="message" role="status">${flash.message}</div>
			</g:if>
			<g:if test="${flash.error}">
			<div class="errors" role="alert">${flash.error}</div>
			</g:if>
			
			<g:hasErrors bean="${datasetInstance}">
			<ul class="errors" role="alert">
				<g:eachError bean="${datasetInstance}" var="error">
				<li <g:if test="${error in org.springframework.validation.FieldError}">data-field-id="${error.field}"</g:if>><g:message error="${error}"/></li>
				</g:eachError>
			</ul>
			</g:hasErrors>
			
<%--			<p class="tooltip-ui" style="margin: 0em 1.5em;">--%>
<%--				Have you created documentation for this data set?--%>
<%--				&nbsp;--%>
<%--				If not, you can refer to the <a target="_blank" href="${createLink(uri: '/docFormatGuide')}">documentation and format guidelines</a> for assistance.--%>
<%--			</p>--%>
			
			
			<form iname="readme">
				<fieldset class="dialog">
					<div id="dialog-form" title="In Order to Create Your Readme, Please Tell Us...">
						<p class="validateTips">All form fields are required.</p>
					 
						<label for="filename">File Name &nbsp;&nbsp;&nbsp;<em>(Without File Extension)</em></label>
						<input type="text" id="filename" name="filename" class="text ui-widget-content ui-corner-all" />
						
						<label for="ftype">File Type &nbsp;&nbsp;&nbsp;<em>(Select One)</em></label>
						<div id="filetypes">
							<input type="radio" id="ftype1" name="ftype" value="PDF"><label for="ftype1">PDF</label>
							<input type="radio" id="ftype2" name="ftype" value="HTML"><label for="ftype2">HTML</label>
							<input type="radio" id="ftype3" name="ftype" value="ASCII"><label for="ftype3">ASCII Text</label>
							<!--<input type="radio" id="ftype4" name="ftype" value="DOC"><label for="ftype4">DOC</label>-->
						</div>
						<br />
						
						<label for="dopt">Readme Creation Options &nbsp;&nbsp;&nbsp;<em>(Select One or Both)</em></label>
						<div id="doptions">
						  <input type="checkbox" id="dopt1" name="dopt" value="download" /><label for="dopt1">Download Readme</label>
						  <input type="checkbox" id="dopt2" name="dopt" value="add_file" /><label for="dopt2">Add Readme to Data Set</label>
						</div>
					  
					</div>
				</fieldset>
			</form>
			
			
<%--			<g:form controller="dataset" action="docWizardDownload" onSubmit="var fname = prompt('Enter the readme file name:'); if (fname != null) { document.getElementById('readmeFilename').value = fname; } return true;" enctype="multipart/form-data">--%>
			<g:form name="create-readme-form" controller="dataset" action="docWizardDownload" onSubmit="" enctype="multipart/form-data">
				<fieldset class="form">
					<ckeditor:editor name="readme_content" height="600px" width="100%">
						${generatedReadme}
					</ckeditor:editor>
				</fieldset>
				<br />
				<fieldset class="buttons">
					<g:hiddenField name="id" value="${datasetInstance?.id}" />
<%--					<g:hiddenField id="readmeFilename" name="filename" value="" />--%>
					<g:submitButton class="dataSave" id="create-readme-submit-btn" style="display: none;" before="" name="docWizardDownload" value="Create this Readme File" />
					<input type="button" id="create-readme" onclick="triggerDialog();" value="Create this Readme File" />
					<g:link class="show" action="show" id="${datasetInstance?.id}">Go Back</g:link>
				</fieldset>
			</g:form>
		</div>
		
	</body>
</html>