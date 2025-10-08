<div id="reqList">	
	<g:each var="req" in="${meta.Requirement.findAllByProject(projectInstance)}">
		<g:render template="/requirement/req" model="['prjReq':req, 'clone':'nonclone', 'hidden':false]" />
	</g:each>
</div>
<g:render template="/requirement/req" model="['prjReq':req, 'clone':'clone', 'hidden':true]" />

<g:ifInternalContact domain="project" id="${projectInstance?.id}">
	<br />
	<g:render template="/requirement/new" />
</g:ifInternalContact>