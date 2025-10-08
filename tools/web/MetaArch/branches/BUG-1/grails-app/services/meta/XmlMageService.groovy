package meta

import meta.auth.*
import groovy.xml.*
import groovy.util.slurpersupport.*
import org.cyberneko.html.parsers.SAXParser //NekoHTML

import java.util.regex.*
import java.util.*
//import groovy.util.slurpersupport.GPathResult;
//import groovy.xml.MarkupBuilder

import org.codehaus.groovy.grails.web.context.ServletContextHolder

class XmlMageService {

    def springSecurityService
	def CurrentUserService currentUserService
	def servletContext = ServletContextHolder.servletContext

    //def serviceMethod() { }
	
	def hasXmlTemplate(Project project) {
		// Displays the contents of the XML Metadata Template/Profile as a feature for projects.
		if (project != null) {
			def xmlTemplate = project.additionalMetadata
			
			if (xmlTemplate != null) {
				return true
			}
			
			return false
		}
	}
	
	def getAllFields(Project project) {
		def fields = []
		def xml
		
		if (project.additionalMetadata != null) {
			xml = getRawXml(project.additionalMetadata)
		} else {
			return []
		}
		//println xml
		
		def root =  new XmlSlurper().parseText(xml)
		
		def fieldNodes = root.'**'.findAll { it.@type != '' }
		fieldNodes.each { fn ->
			fields.add(fn.name())
		}
		
		//println fields
		return fields
	}
	
	def getRawXml(XmlTemplate xmlTemplate) {
		def rawXml = ""
		
		if ( xmlTemplate != null ) {
			if ( xmlTemplate.file != null ) {
				new ByteArrayInputStream( xmlTemplate.file ).eachLine('UTF-8') { line ->
					rawXml += line + "\n"
				}
			} else if ( xmlTemplate.body != null ) {
				rawXml = xmlTemplate.body
			}
		}
		
		return rawXml
	}
	
	/*
	def createFormFromXml(Project project, Dataset dataset) {
		// Reads the XML Metadata Template/Profile and creates the appropriate form fields for these metadata.
		
	}
	*/
	
	def convertFormInputToXml(Project project, Dataset dataset, String formInput) {
		/*
		// Reads the user input from the generated form and converts it back into XML for storage in the database.
		def writer = new StringWriter()
		def formParser
		def xml = new MarkupBuilder(writer)
		
		if (project != null && dataset !=null && formInput != '') {
			formParser = new XmlSlurper().parseText(formInput)
			
			readXmlChild(formParser)			
		}
		// If any of the input parameters are NULL, do nothing here!	
		*/
		
		if (project != null && dataset != null && formInput != '') {
			convertHTMLtoXML(formInput)
		}
	}
	
	
	
	
	
	
	/*
	def xmlForm = {attrs ->
		def writer = new StringWriter()
		def xml = new MarkupBuilder(writer)
		//def file = "web-app/docs/PacMARS.xml"
		
		//def xmlFile = ''
		def xmlFile = XmlTemplate.get(attrs.xmlFile.id)
		def xmlText = ''
		
		if (xmlFile != null) {
			
			if (xmlFile.file != null) {
				new ByteArrayInputStream( xmlFile.file ).eachLine('UTF-8') { line ->
					xmlText += line + "\n"
				}
			} else if (xmlFile.body != '') {
				xmlText = xmlFile.body
			}
			
			def projects = new XmlSlurper().parseText(xmlText)
			
			out << "<div id=\"add-project-metadata\">\n"
			out << readXmlChild(projects)
			out << "</div> <!-- end of project-specific metadata -->\n"
		}
	}
	*/	
	
	
	
	private String readXmlChild(GPathResult xmlChild) {
		def sw = new StringWriter()
		def sb = new StringBuilder()
		def xml = new MarkupBuilder(sw)
		def displayLabel = ''
		def norecurse = false
		
		def xmlName = null
		def xmlAttrs = null
		Map<String,String> xmlAttrMap = new HashMap<String, String>();
		
		
		// Find the xml object name and save it
		if (xmlChild.name() == 'fieldset') {
			xmlName = xmlChild.@id
			xmlName = (xmlName =~ /-children/).replaceAll("")
		} else if (xmlChild.name() == 'div' && xmlChild.@"domain-property" != null) {
			xmlName = xmlChild.@"domain-property"
		} else if (xmlChild.name() == 'label') {
			// Do nothing
			displayLabel = xmlChild.text()
			return null // Shouldn't contain anything?
		}
		
		// Find any and all xml object attributes
		if (xmlChild.@"all-attr" != null) {
			xmlAttrs = xmlChild.@"all-attr"
			xmlAttrs = (xmlAttrs =~ /[\[\]]/).replaceAll("")
			xmlAttrs = xmlAttrs.split(/,\s/) // Should now be an array of xml object attributes to add
			
			xmlAttrs.each { p ->
				println "p = \"" << p << "\""
				p = p.split(/: /)
				println "k = " << p[0] << ", v = " << p[1]
				p[1] = (p[1] =~ /\'/).replaceAll("")
				xmlAttrMap.put(p[0], p[1])
			}
		}
		
		
		/*
		// Current Attribute Types: name, type, display, hidden
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
					if (xmlChild.@textArea != null) {
						//elemOutput = '<g:textField name="'+ xmlChild.name() + '" value="'+ xmlChild.@value +'" />'
						//<g:textArea name="summary" cols="40" rows="5" maxlength="65535" value="${datasetInstance?.summary}"/>
						elemOutput = g.textArea(name: xmlChild.name(), maxlength: 65535, value: xmlChild.@value)
					} else {
						//elemOutput = '<g:textField name="'+ xmlChild.name() + '" value="'+ xmlChild.@value +'" />'
						elemOutput = g.textField(name: xmlChild.name(), value: xmlChild.@value)
					}
					break
					
				case "Text":
					//elemOutput = '<g:textArea name="'+ xmlChild.name() +'" value="'+ xmlChild.@value +'" rows="5" cols="40" />'
					elemOutput = g.textArea(name: xmlChild.name(), value: xmlChild.@value)
					break
			
				case "Date":
					//elemOutput = '<g:datePicker name="'+ xmlChild.name() +'" value="'+ xmlChild.@value +'" precision="minute" ></g:datePicker>'
					def dateValue
					if (xmlChild.@value && xmlChild.@value != '') { dateValue = new Date(xmlChild.@value) }
					else { dateValue = new Date() }
					
					elemOutput = g.datePicker(
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
					sb << "\">"
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
		
		*/
		
		
		if (xmlName == null) { xmlName = 'unknownObj' }
		
		// Check for children and recurse to them if needed.
		if (xmlChild.children().size() != 0 && norecurse == false) {
			xmlChild.children().each { xChild ->
				xml."$xmlName"( xmlAttrMap, readXmlChild(xChild) )
			}
		} else {
			xml."$xmlName"(xmlAttrMap)
		}
		
		println sw
		return sw //xml
	}
	
	private String buildSelectList(GPathResult listRoot) {
		def listItems = listRoot.children().findAll { it.@type == 'ListItem' }
		def output = ''
		
		output += '<select domain-property-type="'+listRoot.@type+'" name="'+listRoot.name()+'" id="'+listRoot.name()+'">\n'
		
		listItems.each { l ->
			output += '\t<option domain-property-type="'+l.@type+'" value="'+l.text()+'"'
			if (l.@selected == true) {
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
				if (output != '[') {
					output += ', '
				}
				output += k + ': \''
				output += v + '\''
			}
			output += ']'
			
		}
		
		return output
	}
	
	
	
	
	
	
	/* 
	 * Converted from xmlmage.js (JavaScript) **********************************
	 */
	/**/
	String convertHTMLtoXML(String htmlString) {
		// See BuildConfig.groovy for the dependency-errors bug fix!
		@Grapes(
			@Grab(group='net.sourceforge.nekohtml', module='nekohtml', version='1.9.17')
		)
		
		def saxParser = new org.cyberneko.html.parsers.SAXParser()
		saxParser.setFeature('http://xml.org/sax/features/namespaces',false)
		saxParser.setFeature('http://cyberneko.org/html/features/balance-tags/document-fragment',true)
		def rootElement = new XmlSlurper(saxParser).parseText(htmlString)
		
		def xmlOutput = ""
		
		if (rootElement == null || rootElement.text() == "") {
			// we aren't interested in non-existent or empty elements
			return xmlOutput
		}
		
		// Start writing the XML document if rootElement exists and isn't empty
		xmlOutput = renderXMLNode(rootElement, rootElement)
		
		return xmlOutput
	}
	
	private String renderXMLNode(GPathResult node, GPathResult rootNode) {
		def xmlNode = ""
		def tagName = (node.name()).toLowerCase()
		def attributeString = ""
		def attributeArray
		def domainProperty = ""
		
		if ((tagName == "div" || tagName == "fieldset") && !(tagName == "legend" || tagName == "label")) {
			// We can assume that the all-attr attribute is in the fieldset's and div's
			if (node.@"all-attr" != null) {
				attributeString += node.@"all-attr".toString()
				attributeString = attributeString.replaceFirst(/^\[/,"")
				attributeString = attributeString.replaceFirst(/\]$/,"")
				attributeArray = attributeString.split(/\], /)
			}
			
			// Get the domain-property
			if (tagName == "div") {
				if (node.@"domain-property" != null) {
					domainProperty = node.@"domain-property".toString()
				}
			} else if (tagName == "fieldset") {
				domainProperty = node.@"id".toString()
				domainProperty = domainProperty.replaceFirst(/-children/, "")
			}
		}
		
		if (domainProperty != "") {
			xmlNode += "<"+domainProperty
			
			for (def i = 0; i < attributeArray.size(); i++) {
				attributeArray[i] = attributeArray[i].replaceFirst(/^\[/, "")
				attributeArray[i] = attributeArray[i].replaceFirst(/\]$/, "")
				
				def arrElement = attributeArray[i].split(":", 2)
				arrElement[1] = arrElement[1].replaceFirst(/^ '/, "")
				arrElement[1] = arrElement[1].replaceFirst(/'$/, "")
				
				xmlNode += " " + arrElement[0] + "=\"" + arrElement[1] + "\""
			}
			
			def fieldObject
			
			rootNode.'**'.findAll { it.@name.toString() == domainProperty }.each {
				fieldObject = it
			
//				println 'fieldObject: ' << fieldObject.@name.toString() << '\n'				
			}
			
			if (fieldObject != null) {
				def fieldValue = ''
				
				if (fieldObject.@"value" != null) {
					fieldValue = fieldObject.@"value"
				}
				
				if (fieldValue != "") {
					xmlNode += " value=\"" + fieldValue + "\""
				}
			}
			
			xmlNode += ">"
		}
			
			
		// Add inner contents / children here
		node.children().each { childNode ->
			xmlNode += renderXMLNode(childNode, rootNode)
		}
		// Add inner contents / children here
		
		
		if (domainProperty != "") {
			xmlNode += "</"+domainProperty+">"
		}
		
		return xmlNode
	}
	/**/
}
