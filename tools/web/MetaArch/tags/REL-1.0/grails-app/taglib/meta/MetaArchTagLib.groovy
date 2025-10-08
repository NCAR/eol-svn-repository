package meta

import meta.auth.*
import org.codehaus.groovy.grails.plugins.springsecurity.SpringSecurityUtils
import groovy.time.TimeCategory
import groovy.util.slurpersupport.GPathResult;
import groovy.xml.MarkupBuilder

class MetaArchTagLib {
	
	def springSecurityService
	CurrentUserService currentUserService
	DatasetAuthService datasetAuthService
	XmlMageService xmlMageService
	
	def ifMultiProject = {attrs, body ->
		def prjList = currentUserService.currentUserProjects()
		
		if (prjList.size() > 1) {
			out << body()
			return
		}
	}
	
	def ifPermitted = {attrs, body ->
		def per = currentUserService.lookupUser()
		def isAdmin = currentUserService.isAdmin()
		
		if (per) {
			if (isAdmin == true) {
				out << body()
				return
			} else {
				if (attrs.domain == 'project') {
					def project = Project.get(attrs.id)
					def memberList = ProjectMember.executeQuery("select pm.member from ProjectMember pm where pm.member = ? and pm.project = ? and pm.memberType = ?", [per, project, MemberType.INTERNAL_CONTACT])
					
					if (memberList != null) {
						 if (memberList.contains(per)) {
							 out << body()
							 return
						 }
					}
					
					//if (project.internalContact == per || project.creator == per) {
					//	out << body()
					//	return
					//} else if (attrs.allowGroup && attrs.allowGroup == true) {
					//	if (project.editors.containsValue(per)) {
					//		out << body()
					//		return
					//	}
					//}
				} else if (attrs.domain == 'dataset') {
					def dataset = Dataset.get(attrs.id)
					
					//if (dataset.author == per || dataset.pointOfContact == per) {
					if (datasetAuthService.canEditDataset(dataset)) {
						out << body()
						return
					}
				}
			}
		}
	}
	
	def linkTheme = {attrs ->
		def per
		def isAdmin = SpringSecurityUtils.ifAnyGranted('ROLE_ADMIN, ROLE_DEVELOP')
		def project
		def prjTheme = attrs.href
		
		if (springSecurityService.isLoggedIn()) {
			per = (User) springSecurityService.getCurrentUser()
			per.attach()
		}	
		
		if (per == null) {
			return
		} else if (per != null) {
			// If there is a current user that could be retrieved, look for a default project.
			project = per.defaultProject
			
			// If the current controller is project, use the given project's CSS URL if it exists.
			if ( controllerName == "project" && (actionName == "show" || actionName == "edit") ) {
				project = Project.get(params.id)
			} else if ( controllerName == "dataset" && (actionName == "show" || actionName == "edit" || actionName == "editData") ) {
				project = Dataset.get(params.id).project
			} else if ( (controllerName == "dataset" && actionName == "create") || (params.project != null) ) {
				project = Project.findByName(params.project)
			}
		
			if (project != null) {
				// If there is a default project, try to retrieve its CSS URL if it exists.
				prjTheme = project.cssUrl?.value
			} else {
				// Execute Query works around the lazily initialized exception
				def prjMemberList = Project.executeQuery("select distinct pm.project from ProjectMember pm where pm.member = ?", [per])
				def prjList = [] // Instantiates an empty list to add css urls to it
				
				// If user is an admin, give them all the projects!
				if (isAdmin) {
					prjMemberList = Project.list()
				}
				
				prjMemberList.each { prjMemberItem ->
					def prj = Project.get(prjMemberItem.id)
					if (prj.cssUrl != null) {
						prjList.add(prj.cssUrl)
					}
				}

				if (prjList.size() > 0) {
					// If There is at least one item in the list of project CSS URLs, get it.
					prjTheme = prjList.get(0)
				}
			}
		}
		
		//println "params... " + params.project
		// Don't use a project theme on index.gsp, admin.gsp, etc.
		if ( controllerName == null || 
			( controllerName == "user" ) ||
			( controllerName == "dataset" && actionName == "getProject" ) ||
			( controllerName == "dataset" && actionName == "list" && params.project == null ) ||
			( controllerName == "project" && (actionName == "create" || actionName == "list" ) ) ) {
			prjTheme = null
		}
		
		out << """<!-- Begin Project Theme -->"""
		if (prjTheme != null && prjTheme != "") {
			//out << """<link rel="stylesheet" href="${prjTheme}" type="text/css" />"""
			//println """<link rel="stylesheet" href="${prjTheme}" type="text/css" />"""
			
			out << """<style type="text/css">${prjTheme}</style>"""
		}
		out << """<!-- End Project Theme -->"""
	}
	
	def listDatasets = {attrs ->
		int displaySize = 10
		def thisProject = Project.findById(attrs.project)
		if (thisProject) {
			out << """<ul>"""
			
			def dsList = thisProject.datasets
			def date = new Date()
			def count = 0
			
			// Sort the list before displaying the first five
			//dsList = dsList.asList().sort{it.lastUpdated}.reverse()
			dsList = dsList.asList().sort{it.dateCreated}.reverse()
			
			//dsList.each { Dataset ds ->
			for (ds in dsList) {
				// Only show the most recent datasets based upon the displaySize.
				if (count >= displaySize) { break } else { count += 1 }
				
				def elapsedTime
				use (TimeCategory) {
					//elapsedTime = date - ds.lastUpdated
					elapsedTime = date - ds.dateCreated
				}
				def dsLink = createLink(controller: 'dataset', action:'show', id: ds.id)
				out << """
					<li><a href="${dsLink}">${ds.title}</a> (${elapsedTime} ago)</li>
					"""
			}
			if (dsList.size() > displaySize) {
				out << """</ul>
					<br />
					<a href="/metaarch/dataset/list?project=${thisProject.name}">+ View All +</a>
					"""
			}
		} else {
			out << """
				Unable to find Dataset List. Sorry about that!
				"""
		}
	}
	
	def listProjectEditors = {attrs ->
		def isAdmin = currentUserService.isAdmin()
		def thisProject = Project.findById(attrs.project)
		if (thisProject) {
			out << """<ul>"""
			
			thisProject.members().each { User per ->
				def mType = ProjectMember.findByMemberAndProject(per, thisProject).memberType
				def delImg = "<img src=\"" << resource(dir: 'images', file: 'delete.png') << "\" />"
				def modImg = "<img src=\"" << resource(dir: 'images/skin', file: 'database_edit.png') << "\" />"
				def delLink = "<a href=\"" << g.createLink(controller: "project", action:"modifyGroup", params:[projectId: thisProject.id, username: per.username, memberType: mType, cmd: 'delete']) << "\">" << delImg << "</a>"
				def modLink = "<a href=\"" << g.createLink(controller: "project", action:"modifyGroup", params:[projectId: thisProject.id, username: per.username, memberType: mType, cmd: 'modify']) << "\">" << modImg << "</a>"
				modLink = "" // Remove this line once modifyGroup for "update" cmd is developed!!!
				out << """<li>""" 
				if (isAdmin) { out << modLink }
				out << """&nbsp; ${per.realname} ($mType) &nbsp;"""
				if (isAdmin) { out << delLink }
				out << """</li>"""
			}
			out << """</ul>"""
		} else {
			out << """
				Unable to find Member List. Sorry about that!
				"""
		}
	}
	
	def listApprovalRequests = {attrs ->
		def isAdmin = currentUserService.isAdmin()
		
		if (isAdmin == true) {
			def userList = User.findAllByEnabledAndAccountLocked(false, true)
			
			if (userList.size() > 0) {
				out << """
						<!-- Display New User Requests -->
						<div class="project-permitted user-requests" style="float: left;">
							<h4>Registration Approval Requests</h4>
							<p style="clear: both;"></p>
						"""
			
				userList.each { u ->
					def uLink = g.createLink(controller: 'user', action: 'show', id: u.id, class: 'float-show')
					out << '<a href="' << uLink << '">' << u.toString() << '</a>\n'
					out << '<br />\n'
				}
				
				out << """
						</div>
						<!-- End Display New User Requests -->
						"""
			}
		}
	}
	
	def projectBox = {attrs ->
		def project = attrs.project
		if (project == null) { return }
		
		// If you get this far, then it can be assumed that project is not null
		def sb = new StringBuilder()
		
		//create a links to edit and delete the contact by setting the actions and ids
		def showLink = g.createLink(controller: "project", action:"show", id:project.id)
		def createLink = g.createLink(controller: "dataset", action:"create", params:[project: project.name])
		
		def dsCount = project.datasets.size()
				
		sb << """
			<div class="prj">
	            <div class="prjtop">
	              <span class="name">
			"""
			
		//if (attrs.forDataset) { //(attrs.forDataset != null && attrs.forDataset == true) {
		if (attrs.forDataset != null && attrs.forDataset == "true") {
			sb << '<a href="' << createLink << '">'
		} else {
			sb << '<a href="' << showLink << '">'
		}
				  
		sb << """
							${project.name}
						  </a>
					</span>
				</div>
				<div class="prjbox">
					${project.fullName}
					<br />
					${project.fundingAgency} ${project.awardNumber}
					<br />
					${dsCount} Data Sets
					<br />
		"""
		if (project.summary != null) {
			sb << """
					${project.summary}
					<br />
				"""
		}
		sb << """
	            </div>
			</div>
        """
 
		out << sb as String
	}
	
	def selectMemberType = {attrs ->
		def memberTypeList = MemberType.values()
		def sb = new StringBuilder()
		
		sb << '<select id="memberType-list" name="memberType" class="many-to-one"> \n'
		
		memberTypeList.each { mt ->
			sb << '\t <option value="' << mt.name() << '">'
			sb << mt.toString() << '</option> \n'
		}
		
		sb << '</select>'
		
		out << sb as String
	}
	
	def selectProject = { attrs ->
		// How can this tag be modified to restrict the available authors to currently logged in user (or all
		// if an admin)?
		def per = User.get(springSecurityService.getCurrentUser().id)
		def isAdmin = currentUserService.isAdmin()
		def prjList = []
		
		if (per != null) {
			if (isAdmin == false) {
				def prjMemberList = Project.executeQuery("select distinct pm.project from ProjectMember pm where pm.member = ?", [per])
				
				prjMemberList.each { p ->
					prjList.add(p)
				}
			} else {
				Project.list().each { p ->
					prjList.add(p)
				}
			}
		}
		
		if (prjList.size() > 0) {
			def sb = new StringBuilder()
				
				sb << '<select id="' << attrs.id << '" class="many-to-one"'
				if (attrs.required) sb << ' required=""'
	 			sb << ' name="' << attrs.name << '"> \n'
				 
				prjList.each { prj ->
					sb << '\t <option value="' << prj.id << '"> '
					sb << prj.toString() << ' </option> \n'
				}
				
				sb << '</select> \n'
				
				out << sb as String
		} else {
			// Render not permitted for any projects error.
		}
	}
	
	def user = {attrs ->
		def per = currentUserService.lookupUser()
		def perName
		
		if (per != null) {
			if (per.realname != null) {
				perName = per.realname
			} else {
				perName = per.username
			}
		} else {
			perName = "anonymous"
		}
		
		out << perName as String
	}
	
	def xmlContent = {attrs ->
		def writer = new StringWriter()
		def xml = new MarkupBuilder(writer)
		//def file = "web-app/docs/PacMARS.xml"
		
		//def xmlFile = ''
		def xmlFile = XmlTemplate.get(attrs.xmlFile.id)
		def xmlText = ''
		
		if (xmlFile != null) {
			
			if (xmlFile.body != null && xmlFile.body != '') {
				xmlText = xmlFile.body
			} else if (xmlFile.file != null) {
				new ByteArrayInputStream( xmlFile.file ).eachLine('UTF-8') { line ->
					xmlText += line + "\n"
				}
			}
			
			out << xmlText
		}
	}
	
	def xmlFile = {attrs ->
		def xmlTemplate = XmlTemplate.get(attrs.id)
		if (xmlTemplate != null) {
			if (xmlTemplate.file != null) {
				
				out << """<pre style='white-space: pre-wrap;'>"""
				
				new ByteArrayInputStream( xmlTemplate.file ).eachLine('UTF-8') { line ->
					line = (line =~ /[<]/).replaceAll("&lt;")
					line = (line =~ /[>]/).replaceAll("&gt;")
				    out << line << "\n"
				}
				
				out << """</pre>"""
			}
		}
	}
	
	def xmlForm = {attrs ->
		def writer = new StringWriter()
		def xml = new MarkupBuilder(writer)
		
		def xmlFile
		def xmlText = ''
		def projects
		
		if (attrs.xmlFile != null && attrs.xmlFile.id != null) {
//			log.info("[xmlForm] xmlFile is: "+attrs.xmlFile + "\n")
//			log.info("[xmlForm] xmlFile.id is: "+attrs.xmlFile.id + "\n")
			xmlFile = XmlTemplate.get(attrs.xmlFile.id)
		} else if (attrs.xmlText != null) {
			xmlText = attrs.xmlText
		}
		
		if (xmlFile != null) {
			// Try to parse the xmlFile attribute
//			log.info("trying to parse the xmlFile...\n")
			if (xmlFile.body != null && xmlFile.body != '') {
//				log.info("xmlFile.body is not null\n")
				xmlText = xmlFile.body
			} else if (xmlFile.file != null) {
//				log.info("xmlFile.file is not null\n")
				new ByteArrayInputStream( xmlFile.file ).eachLine('UTF-8') { line ->
					xmlText += line + "\n"
					println line
				} 
			}
			
//			log.info("xmlText is: \n"+xmlText+"\n")
//			println xmlText
			projects = new XmlSlurper().parseText(xmlText)
		} else if (xmlText != '') {
			// Try to parse the xmlText attribute instead
		
			// Reverse the HTML character-representations back to characters
			xmlText = (xmlText =~ /&lt;/).replaceAll("<")
			xmlText = (xmlText =~ /&gt;/).replaceAll(">")
			xmlText = (xmlText =~ /&quot;/).replaceAll("'")
			//println xmlText
			
			projects = new XmlSlurper().parseText(xmlText)
		} else if (xmlFile == null && xmlText == '') {
			out << ""
			return
		}
		
		// Now read the root XML node if it isn't null!
		if (projects != null) {
			out << "<div id=\"add-project-metadata\">\n"
			out << readXmlChild(projects)
			out << "</div> <!-- end of project-specific metadata -->\n"
		}
	}
	
	def xmlShow = {attrs ->
		def writer = new StringWriter()
		def xml = new MarkupBuilder(writer)
		//def file = "web-app/docs/PacMARS.xml"
		
		def xmlFile
		def xmlText = ''
		def projects
		
		if (attrs.xmlFile != null) {
			xmlFile = XmlTemplate.get(attrs.xmlFile.id)
		} else if (attrs.xmlText != null) {
			xmlText = attrs.xmlText
		}
		
		if (xmlFile != null) {
			// Try to parse the xmlFile attribute
			
			if (xmlFile.file != null) {
				new ByteArrayInputStream( xmlFile.file ).eachLine('UTF-8') { line ->
					xmlText += line + "\n"
				} 
			} else if (xmlFile.body != null && xmlFile.body != '') {
				xmlText = xmlFile.body
			}
			
			projects = new XmlSlurper().parseText(xmlText)
		} else if (xmlText != '') {
			// Try to parse the xmlText attribute instead
		
			// Reverse the HTML character-representations back to characters
			xmlText = (xmlText =~ /&lt;/).replaceAll("<")
			xmlText = (xmlText =~ /&gt;/).replaceAll(">")
			xmlText = (xmlText =~ /&quot;/).replaceAll("'")
			//println xmlText
			
			projects = new XmlSlurper().parseText(xmlText)
		}
		
		// Now read the root XML node if it isn't null!
		if (projects != null) {
			out << "<div id=\"show-project-metadata\">\n"
			out << showXmlChild(projects)
			out << "</div> <!-- end of project-specific metadata -->\n"
		}
	}
	
	
	
	
	
	private String showXmlChild(GPathResult xmlChild) {
		def sb = new StringBuilder()
		def displayLabel = ''
		def norecurse = false
		
		// Current Attribute Types: name, type, display, hidden
		// Attribute Type: value - used to store the submitted form values
		if (xmlChild.@hidden != null) {
			
		}
		if (xmlChild.@value != null) {
			def elemOutput = xmlChild.@value
			
			if (xmlChild.@display != '' || ( xmlChild.text() != '' && xmlChild.children().size() == 0 )) {
				displayLabel = xmlChild.@display
				if (displayLabel == '' && xmlChild.children().size == 0) { displayLabel = xmlChild.text() }
				
				if (displayLabel != '' && elemOutput != '') {
					sb << "<div class=\"fieldcontain\" domain-property=\""+xmlChild.name()+"\" all-attr=\""+buildPropertyAttributeList(xmlChild)+"\">\n"
					sb << "\n\t<label for=\""+xmlChild.name()+"\""
					//println (displayLabel.toString().size())
					if (displayLabel.toString().size() > 40) {
						sb << " style=\"width: 100%\""
					}
					sb << ">"
					sb << "\t\t"+displayLabel
					sb << "\t</label>"
				}
			} // end if xmlChild.@display exists
			
			
			if (elemOutput != '') {
				sb << "\t<p>${elemOutput}</p>"
				sb << "</div>"
			} else if (elemOutput == '' && xmlChild.children().size() != 0 && xmlChild.name() != 'domainOptions') {
				sb << "\t<fieldset id=\""+xmlChild.name()+"-children\" class=\"xml-fieldsets\" all-attr=\""+buildPropertyAttributeList(xmlChild)+"\">\n"
				sb << "\t\t<legend>"+displayLabel+"</legend>\n"
				
				xmlChild.children().each { xChild ->
					sb << showXmlChild(xChild) << "\n"
				}
				
				sb << "\t</fieldset> <!-- end of children -->\n"
				norecurse = true // Don't call children again since they've been called once
			}
		} // end if-stmt xmlChild.@type exists
		
		
		// Check for children and recurse to them if needed.
		if (xmlChild.children().size() != 0 && norecurse == false) {
			xmlChild.children().each { xChild ->
				sb << readXmlChild(xChild) << "\n"
			}
		}
		
		return sb as String
	}
	
	private String readXmlChild(GPathResult xmlChild) {
		def sb = new StringBuilder()
		def displayLabel = ''
		def norecurse = false
		
		// Current Attribute Types: name, type, display, hidden
		// Attribute Type: value - used to store the submitted form values
		if (xmlChild.@hidden != null) {
			
		}
		if (xmlChild.@type != null) {
			def elemOutput = ''
			
			// Switch-case for different object types
			switch ( xmlChild.@type ) {
				case "List":
					elemOutput = buildSelectList(xmlChild)
					break
					
				case "ListItem":
					elemOutput = ''
					break
			
				case "String":
					if (xmlChild.@textArea == 'true') {
						//elemOutput = '<g:textField name="'+ xmlChild.name() + '" value="'+ xmlChild.@value +'" />'
						//<g:textArea name="summary" cols="40" rows="5" maxlength="65535" value="${datasetInstance?.summary}"/>
						elemOutput = g.textArea('class': 'dyn-field', name: xmlChild.name(), maxlength: 65535, value: xmlChild.@value)
					} else {
						//elemOutput = '<g:textField name="'+ xmlChild.name() + '" value="'+ xmlChild.@value +'" />'
						elemOutput = g.textField('class': 'dyn-field', name: xmlChild.name(), value: xmlChild.@value)
					}
					break
					
				case "Text":
					//elemOutput = '<g:textArea name="'+ xmlChild.name() +'" value="'+ xmlChild.@value +'" rows="5" cols="40" />'
					elemOutput = g.textArea('class': 'dyn-field', name: xmlChild.name(), value: xmlChild.@value)
					break
			
				case "Date":
					//elemOutput = '<g:datePicker name="'+ xmlChild.name() +'" value="'+ xmlChild.@value +'" precision="minute" ></g:datePicker>'
					def dateValue
					if (xmlChild.@value && xmlChild.@value != '') { dateValue = new Date(xmlChild.@value) }
					else { dateValue = new Date() }
					
					elemOutput = g.datePicker(
									'class': 'dyn-field',
									name: xmlChild.name(),
									value: dateValue,
									precision: "minute")
					break
			
				default:
					elemOutput = ''
			}
			
			if (xmlChild.@display != '' || ( xmlChild.text() != '' && xmlChild.children().size() == 0 )) {
				displayLabel = xmlChild.@display
				if (displayLabel == '' && xmlChild.children().size == 0) { displayLabel = xmlChild.text() }
				
				if (displayLabel != '' && elemOutput != '') {
					sb << "<div class=\"fieldcontain\" domain-property=\""+xmlChild.name()+"\" all-attr=\""+buildPropertyAttributeList(xmlChild)+"\">\n"
					sb << "\n\t<label for=\""+xmlChild.name()+"\""
					//println (displayLabel.toString().size())
					if (displayLabel.toString().size() > 40) { 
						sb << " style=\"width: 100%\""
					}
					sb << ">"
					sb << "\t\t"+displayLabel
					sb << "\t</label>"
				}
			} // end if xmlChild.@display exists
			
			
			if (elemOutput != '') {
				sb << "\t${elemOutput}"
				sb << "</div>"
			} else if (elemOutput == '' && xmlChild.children().size() != 0 && xmlChild.name() != 'domainOptions') {
				sb << "\t<fieldset id=\""+xmlChild.name()+"-children\" class=\"xml-fieldsets\" all-attr=\""+buildPropertyAttributeList(xmlChild)+"\">\n"
				sb << "\t\t<legend>"+displayLabel+"</legend>\n"
				
				xmlChild.children().each { xChild ->
					sb << readXmlChild(xChild) << "\n"
				}
				
				sb << "\t</fieldset> <!-- end of children -->\n"
				norecurse = true // Don't call children again since they've been called once
			}
		} // end if-stmt xmlChild.@type exists
		
		
		// Check for children and recurse to them if needed.
		if (xmlChild.children().size() != 0 && norecurse == false) {
			xmlChild.children().each { xChild ->
				sb << readXmlChild(xChild) << "\n"
			}
		}
		
		return sb as String
	}
	
	private String buildSelectList(GPathResult listRoot) {
		def listItems = listRoot.children().findAll { it.@type == 'ListItem' }
		def output = ''
		
		output += '<select class=\'dyn-field\' domain-property-type="'+listRoot.@type+'" name="'+listRoot.name()+'" id="'+listRoot.name()+'">\n'
		
		listItems.each { l ->
			output += '\t<option domain-property-type="'+l.@type+'" value="'+l.text()+'"'
			if (l.@selected == true || l.@value == true) {
				output += ' selected="true"'
			}
			output += '>'+l.text()+'</option>\n'
		}
		
		output += '</select> <!-- end of '+listRoot.name()+' list -->\n'
		
		return output
	}
	
	private String buildPropertyAttributeList(GPathResult element) {
		def output = ''
		
		if (element.attributes().size() != 0) {
		
			output = '['
			element.attributes().each { k,v ->
				if (k != 'value') {				
					if (output != '[') {
						output += ', '
					}
					output += '[' + k + ': \''
					output += v + '\']'
				}
			}
			output += ']'
			
		}
		
		return output
	}
}
