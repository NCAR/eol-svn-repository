<div id="project-member${i}" class="project-member-div" <g:if test="${hidden}">style="display:none;"</g:if>>
	<g:hiddenField name='projectMemberList[${i}].id' value='${projectMember?.id}' />
	<g:hiddenField name='projectMemberList[${i}].deleted' value='false' />
	<g:hiddenField name='projectMemberList[${i}].new' value='false' />
	<g:hiddenField name='projectMemberList[${i}].project' value='${projectMember?.project}' />
	
	<g:textField name='projectMemberList[${i}].member' value='${projectMember?.member}' />
	<g:select name="projectMemberList[${i}].memberType"
		from="${meta.MemberType.values()}"
		optionValue="value"
		value = "${projectMember?.memberType?.displayName}" />
	
	<span class="del-project-member">
		[ X ]
	</span>
</div>