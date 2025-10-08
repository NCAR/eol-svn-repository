import meta.auth.*
import meta.*

class BootStrap {
	
	def springSecurityService

    def init = { servletContext ->
		if (!Authority.findByAuthority('ROLE_DMG')) {
			def dmgRole = Authority.findByAuthority('ROLE_DMG') ?: new Authority(authority: 'ROLE_DMG').save(failOnError: true)
		}
		
		if(!User.count()) {
			log.info "Creating roles...."
			def userRole = Authority.findByAuthority('ROLE_USER') ?: new Authority(authority: 'ROLE_USER').save(failOnError: true)
			def dmgRole = Authority.findByAuthority('ROLE_DMG') ?: new Authority(authority: 'ROLE_DMG').save(failOnError: true)
			def adminRole = Authority.findByAuthority('ROLE_ADMIN') ?: new Authority(authority: 'ROLE_ADMIN').save(failOnError: true)
			def devRole = Authority.findByAuthority('ROLE_DEVELOP') ?: new Authority(authority: 'ROLE_DEVELOP').save(failOnError: true)
			
			//def password = springSecurityService.encodePassword('password')
			
			log.info "Creating users...."
			def adminUser = User.findByUsername('admin') ?: new User(
				username: 'admin',
				realname: 'Administrator',
				email: 'eol-metaarch@ucar.edu',
				organization: 'NCAR-EOL',
				password: 'group-admin-override', // encodePassword is not necessary for Grails 2.0
				enabled: true).save(failOnError: true)
			if (!adminUser.authorities.contains(adminRole)) {
				UserAuthority.create adminUser, adminRole, true
			}
			if (!adminUser.authorities.contains(devRole)) {
				UserAuthority.create adminUser, devRole, true
			}
			
			adminUser = User.findByUsername('orin') ?: new User(
				username: 'orin',
				realname: 'Amanda Orin',
				email: 'stott@ucar.edu',
				organization: 'NCAR-EOL',
				phoneNumber: '303-497-8764',
				password: 'orin-admin', // encodePassword is not necessary for Grails 2.0
				enabled: true).save(failOnError: true)
			if (!adminUser.authorities.contains(adminRole)) {
				UserAuthority.create adminUser, adminRole, true
			}
			if (!adminUser.authorities.contains(devRole)) {
				UserAuthority.create adminUser, devRole, true
			}
			
			adminUser = User.findByUsername('stott') ?: new User(
				username: 'stott',
				realname: 'Don Stott',
				email: 'stott@ucar.edu',
				organization: 'NCAR-EOL',
				phoneNumber: '303-497-8154',
				password: 'db-stott', // encodePassword is not necessary for Grails 2.0
				enabled: true).save(failOnError: true)
			if (!adminUser.authorities.contains(adminRole)) {
				UserAuthority.create adminUser, adminRole, true
			}
			if (!adminUser.authorities.contains(devRole)) {
				UserAuthority.create adminUser, devRole, true
			}
			
			/**/
			def regUser = User.findByUsername('foobar') ?: new User(
				username: 'foobar',
				realname: 'Foo Test Bar',
				email: 'stott@ucar.edu',
				organization: 'NCAR-EOL',
				password: 'password',
				enabled: true).save(failOnError: true)
			if (!regUser.authorities.contains(userRole)) {
				UserAuthority.create regUser, userRole, true
			}
			
			if(!Agency.count()) {
				log.info "Creating agencies...."
				def agency = Agency.findByName('NSF') ?: new Agency(
					name: 'NSF', 
					description: 'National Science Foundation').save(failOnError: true)
				agency = Agency.findByName('NOAA') ?: new Agency(
					name: 'NOAA', 
					description: 'National Oceanic and Atmospheric Administration').save(failOnError: true)
				agency = Agency.findByName('NASA') ?: new Agency(
					name: 'NASA', 
					description: 'National Aeronautics and Space Administration').save(failOnError: true)
				agency = Agency.findByName('NPRB') ?: new Agency(
					name: 'NPRB', 
					description: 'North Pacific Research Board').save(failOnError: true)
				agency = Agency.findByName('DOE') ?: new Agency(
					name: 'DOE', 
					description: 'Department of Energy').save(failOnError: true)
				agency = Agency.findByName('Other') ?: new Agency(
					name: 'Other', 
					description: '').save(failOnError: true)
			}
			
			if(!Project.count()) {
				log.info "Creating projects...."
				new Project(
					name: 'PacMARS',
					fullName: 'Pacific Marine Arctic Research Synthesis',
					cssUrl: 'http://arctic.eol.ucar.edu/css/pacmars.css',
					fundingAgency: Agency.findByName("NPRB"),
					awardNumber: 'A01/T2201�T2207'
					).save(failOnError: true)
			}
			
			if(!ProjectMember.count()) {
				log.info "Creating project members...."
	//			new ProjectMember(
	//				member: adminUser,
	//				project: adminProject,
	//				memberType: MemberType.INTERNAL_CONTACT
	//				).save(failOnError: true)
	//			new ProjectMember(
	//				member: User.findByUsername('test1'),
	//				project: adminProject,
	//				memberType: MemberType.PI
	//				).save(failOnError: true)
	//			new ProjectMember(
	//				member: adminUser,
	//				project: regProject,
	//				memberType: MemberType.INTERNAL_CONTACT
	//				).save(failOnError: true)
				new ProjectMember(
					member: User.findByUsername('foobar'),
					project: Project.findByName('PacMARS'),
					memberType: MemberType.PI
					).save(failOnError: true)
			}
			
			if(!Dataset.count()) {
				log.info "Creating datasets...."
				/* 
				new Dataset(
					title: 'acadis-1',
					author: adminUser,
					project: adminProject,
					fundingAgency: Agency.findByName("NSF"),
					awardNumber: '123456').save(failOnError: true)
				new Dataset(
					title: 'acadis-2',
					author: adminUser,
					project: adminProject,
					fundingAgency: Agency.findByName("NPRB"),
					awardNumber: '123456').save(failOnError: true)
				new Dataset(
					title: 'acadis-3',
					author: adminUser,
					project: adminProject,
					fundingAgency: Agency.findByName("NASA"),
					awardNumber: '123456').save(failOnError: true)
				new Dataset(
					title: 'acadis-4',
					author: adminUser,
					project: adminProject,
					fundingAgency: Agency.findByName("NOAA"),
					awardNumber: '123456').save(failOnError: true)
				new Dataset(
					title: 'acadis-5',
					author: adminUser,
					project: adminProject,
					fundingAgency: Agency.findByName("NSF"),
					spatialType: "point",
					progress: "complete",
					accessRestrictions: "No restrictions",
					awardNumber: '123456').save(failOnError: true)
				new Dataset(
					title: 'acadis-6',
					author: adminUser,
					project: adminProject,
					fundingAgency: Agency.findByName("NSF"),
					spatialType: "point",
					progress: "complete",
					accessRestrictions: "No restrictions",
					awardNumber: '123456').save(failOnError: true)
				new Dataset(
					title: 'test-1',
					author: regUser,
					project: regProject,
					fundingAgency: Agency.findByName("NSF"),
					spatialType: "point",
					progress: "complete",
					accessRestrictions: "No restrictions",
					awardNumber: '123456').save(failOnError: true)
				 */
			}
			
			if(!Format.count()) {
				log.info "Creating formats...."
				new Format(name: 'Unknown', description: 'Unknown/Unspecified Format').save(failOnError: true)
				new Format(name: 'TXT', description: 'ASCII Text').save(failOnError: true)
				new Format(name: 'DOC', description: 'Microsoft Word Document').save(failOnError: true)
				new Format(name: 'HTML', description: 'HyperText Markup Language').save(failOnError: true)
				new Format(name: 'SHP', description: 'ArcGIS Shape File').save(failOnError: true)
				new Format(name: 'XLS', description: 'Excel Spreadsheet').save(failOnError: true)
				new Format(name: 'CSV', description: 'Comma Separated Value').save(failOnError: true)
			}
			
			log.info "...done.\n"
			/**/
		}
    }
    def destroy = {
    }
}
