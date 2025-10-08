package meta

import grails.test.mixin.*
import org.junit.*

/**
 * See the API for {@link grails.test.mixin.services.ServiceUnitTestMixin} for usage instructions
 */
@TestFor(XmlMageService)
class XmlMageServiceTests {
	
	/*
    void testReadXml() {
		// grails test-app -echoOut -unit XmlMageService

		XmlMageService xmlMage = new XmlMageService()
	
		assertNotNull xmlMage
			
		def xmlFile = "web-app/docs/PacMARS.out.xml"
		
		//def xmlFile = XmlTemplate.get(attrs.xmlFile.id)
		def xmlText = ''
		
		if (xmlFile != null) {
			
			*
			if (xmlFile != null) {
				new ByteArrayInputStream( xmlFile ).eachLine('UTF-8') { line ->
					xmlText += line + "\n"
				}
			} else if (xmlFile.body != '') {
				xmlText = xmlFile.body
			}
			*
			
			//def projects = new XmlSlurper().parseText(xmlText)
			def projects = new XmlSlurper().parse(xmlFile)
			
			//out << readXmlChild(projects)
			def newXml = xmlMage.readXmlChild(projects)
			
			println "\n" << newXml.toString()
		}
		//xmlMage.readXml()
    }
	*/
	
	void testConvertXml() {
		// grails test-app -echoOut -unit XmlMageService
		
		XmlMageService xmlMage = new XmlMageService()
	
		assertNotNull xmlMage
				
		
		def htmlText = '''
<fieldset id="project-children" class="xml-fieldsets" all-attr="[[name: 'PacMARS']]">
                <legend></legend>
        <fieldset id="cruiseInfo-children" class="xml-fieldsets" all-attr="[[display: 'Cruise Information']]">
                <legend>Cruise Information</legend>
<div class="fieldcontain" domain-property="cruise" all-attr="[[display: 'Cruise'], [type: 'String']]">

        <label for="cruise">            Cruise  </label>        <input class="dyn-field" name="cruise" value="Testing Cruise" id="cruise" type="text"></div>
        </fieldset> <!-- end of children -->

        <fieldset id="mapserverOptions-children" class="xml-fieldsets" all-attr="[[display: 'Mapserver Options']]">
                <legend>Mapserver Options</legend>
<div class="fieldcontain" domain-property="displayPreference" all-attr="[[textArea: 'true'], [display: 'What is the best way to display this data and how should the data be visualized? (i.e. plot chlorophyll on a color scale from blues to greens to yellows to reds or plot biomass with graduated circles in one color)'], [type: 'String']]">

        <label for="displayPreference" style="width: 100%">             What is the best way to display this data and how should the data be visualized? (i.e. plot chlorophyll on a color scale from blues to greens to yellows to reds or plot biomass with graduated circles in one color)   </label>
<textarea value="Testing q1" class="dyn-field" name="displayPreference" maxlength="65535" id="displayPreference"></textarea></div>
<div class="fieldcontain" domain-property="dataRangePreference" all-attr="[[textArea: 'true'], [display: 'What are the ranges for the data to be plotted? (i.e. biomass 10-19, 20-29, 30-39 with larger circles for larger numbers or evenly spread chlorophyll values across color scale with 25 different colors on a blue-green-yellow-orange-red color scale)'], [type: 'String']]">

        <label for="dataRangePreference" style="width: 100%">           What are the ranges for the data to be plotted? (i.e. biomass 10-19, 20-29, 30-39 with larger circles for larger numbers or evenly spread chlorophyll values across color scale with 25 different colors on a blue-green-yellow-orange-red color scale)     </label>        <textarea value="Testing q2" class="dyn-field" name="dataRangePreference" maxlength="65535" id="dataRangePreference"></textarea></div>
<div class="fieldcontain" domain-property="additionalVisualPreference" all-attr="[[textArea: 'true'], [display: 'Are there additional ways the data can be visualized? Please list them in order of preference.'], [type: 'String']]">

        <label for="additionalVisualPreference" style="width: 100%">            Are there additional ways the data can be visualized? Please list them in order of preference.      </label>        <textarea value="Testing q3" class="dyn-field" name="additionalVisualPreference" maxlength="65535" id="additionalVisualPreference"></textarea></div>
        </fieldset> <!-- end of children -->

        </fieldset> <!-- end of children --> 
						'''
		
		def newXml = xmlMage.convertHTMLtoXML(htmlText)
		
		println "\n" << newXml.toString()
	}
}
