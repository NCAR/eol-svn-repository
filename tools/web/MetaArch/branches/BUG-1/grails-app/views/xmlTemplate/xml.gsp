<%@ page contentType="text/html;charset=UTF-8" %>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'XmlTemplate.label', default: 'XmlTemplate')}" />
		<title><g:message code="default.show.label" args="[entityName]" /> - File Contents</title>
		
		<style type="text/css">
			pre {
				overflow-x: auto; /* Use horizontal scroller if needed; for Firefox 2, not needed in Firefox 3 */
				white-space: pre-wrap; /* css-3 */
				white-space: -moz-pre-wrap !important; /* Mozilla, since 1999 */
				white-space: -pre-wrap; /* Opera 4-6 */
				white-space: -o-pre-wrap; /* Opera 7 */
				/* width: 99%; */
				word-wrap: break-word; /* Internet Explorer 5.5+ */
			}
			#xml-XmlTemplate-contents, #xml-XmlTemplate-contents-2 {
				padding: 5px 15px 5px 30px;
			}
			fieldset {
				border: 1px solid #e7e7e7;
			}
		</style>
	</head>
<body>
		<a href="#xml-XmlTemplate" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		<div id="xml-XmlTemplate" class="content" role="main">
			<h1>
				<g:if test="${xmlTemplateInstance?.filename == ''}">XmlTemplate </g:if>
				File Contents 
				<g:if test="${xmlTemplateInstance?.filename}">of <g:fieldValue bean="${xmlTemplateInstance}" field="filename"/></g:if>
			</h1>
			<g:if test="${flash.message}">
				<div class="message" role="status">${flash.message}</div>
			</g:if>
			<div id="xml-XmlTemplate-contents">
				<g:if test="${xmlTemplateInstance?.file}">
					<g:xmlFile id="${xmlTemplateInstance.id}" />
					<br />
				</g:if>
			</div>
			<h1>
				<g:if test="${xmlTemplateInstance?.filename == ''}">XmlTemplate </g:if>
				Generated Form
				<g:if test="${xmlTemplateInstance?.filename}">of <g:fieldValue bean="${xmlTemplateInstance}" field="filename"/></g:if>
			</h1>
			<div id="xml-XmlTemplate-contents-2">
				<g:if test="${xmlTemplateInstance}">
					<g:xmlForm xmlFile="${xmlTemplateInstance}" />
					<br />
				</g:if>
			</div>
		</div>
		
		<g:form>
			<fieldset class="buttons">
				<g:hiddenField name="id" value="${xmlTemplateInstance?.id}" />
				<g:link class="show" action="show" id="${xmlTemplateInstance?.id}">Show Full Details</g:link>
				<g:actionSubmit class="download" action="download" value="Download File" />
			</fieldset>
		</g:form>
</body>
</html>