<%@ page contentType="text/html;charset=UTF-8" %>
<html>
	<head>
		<meta name="layout" content="main">
		<g:set var="entityName" value="${message(code: 'Dataset.label', default: 'Dataset')}" />
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
			#txtDataset-contents, #txtDataset-contents-2 {
				padding: 5px 15px 5px 30px;
			}
			fieldset {
				border: 1px solid #e7e7e7;
			}
		</style>
	</head>
<body>
		<a href="#txtDataset" class="skip" tabindex="-1"><g:message code="default.link.skip.label" default="Skip to content&hellip;"/></a>
		<div class="nav" role="navigation">
			<g:render template="nav"/>
		</div>
		
		<div class="sub-nav nav" role="navigation">
			<ul>
				<g:if test="${datasetInstance?.isSubmitted == false}">
					<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
						<li><g:link action="addToArchive" id="${datasetInstance?.id}">Submit to Archive</g:link></li>
					</g:ifPermitted>
				</g:if>
				<g:else>
					<g:ifPermitted domain="dataset" id="${datasetInstance?.id}">
						<li><span style="display: block; color: #666; padding: 0.25em 0.7em; font-style: italic;">Already Submitted</span></li>
					</g:ifPermitted>
				</g:else>
				<li>
					<g:link class="clone" action="template" id="${datasetInstance?.id}">Copy this Data Set</g:link>
				</li>
				<sec:ifAnyGranted roles="ROLE_ADMIN,ROLE_DEVELOP,ROLE_DMG">
				<li style="float: right;">
					<g:link class="show" action="show" id="${datasetInstance?.id}">Switch to Normal View</g:link>
				</li>
				</sec:ifAnyGranted>
			</ul>
		</div>
		
		<div id="txtDataset" class="content" role="main">
			<h1>
				Data Set Metadata - Formatted View
			</h1>
			<g:if test="${flash.message}">
				<div class="message" role="status">${flash.message}</div>
			</g:if>
			<div id="txtDataset-contents">
				<pre>${datasetInstanceOutput}</pre>
				<br />
			</div>
		</div>
		
<%--		<g:form>--%>
<%--			<fieldset class="buttons">--%>
<%--				<g:hiddenField name="id" value="${datasetInstance?.id}" />--%>
<%--				<g:link class="show" action="show" id="${datasetInstance?.id}">Show Full Details</g:link>--%>
<%--			</fieldset>--%>
<%--		</g:form>--%>
</body>
</html>