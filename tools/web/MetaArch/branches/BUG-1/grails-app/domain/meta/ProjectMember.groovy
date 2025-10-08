package meta

import meta.auth.*

class ProjectMember {
	
	User member
	Project project
	MemberType memberType
	
	boolean deleted
	static transients = [ 'deleted' ]

    static constraints = {
    }
	
	static ProjectMember link(member, project, memberType) {
		println(member)
		println(project)
		println(memberType)
		def m = ProjectMember.findByMemberAndProject(member, project)
		if (!m) {
			m = new ProjectMember()
			m.member = member
			m.project = project
			m.memberType = memberType
			//member?.addToProjectGroup(m)
			//project?.addToProjectGroup(m)
			m.save()
		}
		return m
	}
	
	static void unlink(member, project) {
		def m = ProjectMember.findByMemberAndProject(member, project)
		if (m)
		{
			member?.removeFromProjectGroups(m)
			project?.removeFromProjectMembers(m)
			m.delete()
		}
	}
	
	String toString() {
		return "$member.realname ($memberType)"
	}
}
