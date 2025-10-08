<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="ErrorReportSubview">

    <h:dataTable rendered="#{generalManager.errorState}" styleClass="errorTable"
            value="#{generalManager.errorMessages}" var="msg">
        <h:column>
            <h:outputText escape="false" value="<h2>#{msg.type}</h2>" />
            <h:outputText escape="false" value="<h3>Message:</h3>" />
            <h:outputText escape="false" value="<p>#{msg.message}</p>" />
            <h:outputText escape="false" rendered="#{!empty msg.details}" 
                    value="<h3>Details:</h3>" />
            <h:outputText escape="false" rendered="#{!empty msg.details}" 
                    value="<p>#{msg.details}</p>" />
        </h:column>
    </h:dataTable>

</f:subview>
