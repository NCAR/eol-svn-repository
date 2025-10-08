
import org.codehaus.groovy.grails.commons.ApplicationHolder
import javax.xml.transform.Source
import javax.xml.transform.sax.SAXSource
import org.xml.sax.InputSource
import javax.xml.validation.*
import org.xml.sax.SAXException
import javax.xml.XMLConstants
import org.xml.sax.ErrorHandler


class ThreddsXmlService {

    static boolean transactional = false

    private static Schema _schema = null  // javadocs encourage us to share & reuse this



    private Schema getSchema() {
      if (_schema) return _schema

      synchronized(_schema) {
       if (_schema) return _schema
       try {
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
        URL schemaLocation = this.getClass().getResource('InvCatalog.1.0.xsd')
        _schema = factory.newSchema(schemaLocation)
        } catch (Exception ex) {
          _schema = null 
          }
       }

      return _schema
      }



    def validate(String xmlString) {

      def errors = null

      try {

      Validator validator = schema.newValidator()

      ErrorHandler eh = new CementSAXErrorHandler()
      validator.setErrorHandler(eh)

      StringReader sr = new StringReader(xmlString)
      InputSource is = new InputSource(sr)

      Source source = new SAXSource(is)

      synchronized( validator ) {
       validator.validate(source)
       }

      errors = eh.getErrors()

      } catch (Exception ex) {
          //log.debug("xml invalid: " + ex.getMessage())
          System.err.println("xml invalid: " + ex.getMessage())
          errors = ex.getMessage()
          }

    return errors
    }

}
