
import org.codehaus.groovy.grails.commons.ApplicationHolder
import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import org.xml.sax.InputSource
import javax.xml.validation.*
import org.xml.sax.SAXException
import javax.xml.XMLConstants
import org.xml.sax.ErrorHandler
//import Cement.CementSAXErrorHandler


class XmlService {
	
    boolean transactional = true

    def validate(String xmlString) {

      def errors = null

      try {

      SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)

      //def applicationContext = ApplicationHolder.application.parentContext
      //File schemaLocation = applicationContext.getResource('classpath:schemata/InvCatalog.1.0.xsd').getFile()
      URL schemaLocation = this.getClass().getResource('InvCatalog.1.0.xsd')

      Schema schema = factory.newSchema(schemaLocation)

      Validator validator = schema.newValidator()

      ErrorHandler eh = new CementSAXErrorHandler()
      validator.setErrorHandler(eh)

      StringReader sr = new StringReader(xmlString)
      InputSource is = new InputSource(sr)

      Source source = new SAXSource(is)

      validator.validate(source)

      errors = eh.getErrors()

      } catch (Exception ex) {
          //log.debug("xml invalid: " + ex.getMessage())
          System.err.println("xml invalid: " + ex.getMessage())
	  errors = ex.getMessage()
          }

    return errors
    }

}
