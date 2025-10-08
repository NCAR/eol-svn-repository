<%@ page import="meta.ProjectMember" %>



<div class="fieldcontain ${hasErrors(bean: projectMemberInstance, field: 'member', 'error')} required">
	<label for="member">
		<g:message code="projectMember.member.label" default="Member" />
		<span class="required-indicator">*</span>
	</label>
	<g:select id="member" name="member.id" from="${meta.auth.User.list()}" optionKey="id" required="" value="${projectMemberInstance?.member?.id}" class="many-to-one"/>
</div>

<div class="fieldcontain ${hasErrors(bean: projectMemberInstance, field: 'memberType', 'error')} required">
	<label for="memberType">
		<g:message code="projectMember.memberType.label" default="Member Type" />
		<span class="required-indicator">*</span>
	</label>
	<g:select name="memberType" from="${meta.MemberType?.values()}" keys="${meta.MemberType.values()*.name()}" required="" value="${projectMemberInstance?.memberType?.name()}"/>
</div>

<div class="fieldcontain ${hasErrors(bean: projectMemberInstance, field: 'project', 'error')} required">
	<label for="project">
		<g:message code="projectMember.project.label" default="Project" />
		<span class="required-indicator">*</span>
	</label>
	<g:selectProject id="project" name="project.id" from="${meta.Project.list()}" optionKey="id" required="" value="${projectMemberInstance?.project?.id}" class="many-to-one"/>
</div>

