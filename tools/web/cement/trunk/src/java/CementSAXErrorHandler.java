import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXParseException;

public class CementSAXErrorHandler implements ErrorHandler {

  static StringBuilder sb;

  public CementSAXErrorHandler() {
    sb = new StringBuilder();
    }

  public String getErrors() {
    if (sb.length() == 0)
      return null;
    return sb.toString();
    }

  public void warning(SAXParseException ex) {
    sb.append(ex.getMessage());
  }

  public void error(SAXParseException ex) {
    sb.append(ex.getMessage());
  }

  public void fatalError(SAXParseException ex) {
    sb.append(ex.getMessage());
  }

}
