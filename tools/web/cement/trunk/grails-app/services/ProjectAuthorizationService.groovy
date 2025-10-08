class ProjectAuthorizationService {

    static boolean transactional = false

    def isaPermittedProject(Project project, cdpPrivileges) {
      def permitted = false
      if (project == null) return false
      if (cdpPrivileges == null) return false

      cdpPrivileges.each {
        if ( project.entryID.equals(it) || project.entryID.startsWith(it + '.') ) {
          permitted = true
          return true // just ends the each closure
          }
        }

      return permitted
    }

    def permittedProjects(Boolean admin, cdpPrivileges) {
        def allProjects = Project.list()
        def myProjects = new ArrayList(allProjects.size())

        if (admin) return allProjects.sort()

        allProjects.each { project ->
             for (entry in cdpPrivileges) {
                if ( project.entryID.equals(entry) || project.entryID.startsWith(entry + '.') ) {
                    myProjects.add(project)
                    break;
                }
            }
        }

        return myProjects.sort()
    }

}
