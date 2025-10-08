class Projlink {
	Researcher researcher
	Project project

	static Projlink link(researcher, project) {
		def m = Projlink.findByResearcherAndProject(researcher, project)
		if (!m)
		{
			m = new Projlink()
			researcher?.addToProjlinks(m)
			project?.addToProjlinks(m)
			m.save()
		}
		return m
	}

	static void unlink(researcher, project) {
		def m = Projlink.findByResearcherAndProject(researcher, project)
		if (m)
		{
			researcher?.removeFromProjlinks(m)
			project?.removeFromProjlinks(m)
			m.delete()
		}
	}

	String toString() { "$researcher.userRealName" }
}
