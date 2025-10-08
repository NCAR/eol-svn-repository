<div class="req-div" is-clone="${clone}" <g:if test="${hidden}">style="display:none;"</g:if>>
	<span class="req-field"><g:fieldValue bean="${prjReq}" field="field"/></span>
	<g:ifInternalContact domain="project" id="${projectInstance?.id}">
	<span class="del-req" id="req-${prjReq?.id}">
		<img src="${resource(dir:'images/skin', file:'database_delete.png')}" />
	</span>
	</g:ifInternalContact>
</div>