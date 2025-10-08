<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="dmg" uri="http://eol.ucar.edu/dmg/taglib" %>

<f:subview id="PhaseListView">
<f:loadBundle basename="resources" var="Resources" />
<h:form id="PhaseListForm">

    <dmg:tree depthClasses="categoryListEntry" id="phaseTree"
                rootClass="projectTitle"
                value="#{manager.phaseTreeRoot}"
                var="entry" varNodeToggler="t">
        <f:facet name="openPhaseRoot">
            <h:outputText value="#{resources.phaseTitle}" />
        </f:facet>
        <f:facet name="openProject">
            <h:panelGrid columnClasses="categoryTreeIcon,categoryName"
                columns="2" styleClass="categoryListTable"
                onmouseover="hoverCategory(this,true);"
                onmouseout="hoverCategory(this,false);">
                <h:commandLink action="self" actionListener="#{t.toggle}">
                    <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                </h:commandLink>
                <h:panelGroup>
                    <h:outputText value="#{entry.projectId}" />
                </h:panelGroup>
            </h:panelGrid>
        </f:facet>
        <f:facet name="closedProject">
            <h:panelGrid columnClasses="categoryTreeIcon,categoryName"
                columns="2" styleClass="categoryListTable"
                onmouseover="hoverCategory(this,true);"
                onmouseout="hoverCategory(this,false);">
                <h:commandLink action="self" actionListener="#{t.toggle}">
                    <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                </h:commandLink>
                <h:panelGroup>
                    <h:outputText value="#{entry.projectId}" />
                </h:panelGroup>
            </h:panelGrid>
        </f:facet>
        <f:facet name="leafPhase">
            <h:panelGrid columnClasses="categoryTreeIcon,categoryName,icon,icon,icon" 
                onmouseover="hoverCategory(this,true);"
                onmouseout="hoverCategory(this,false);"
                columns="5" styleClass="#{manager.phase.phaseId == entry.phaseId ? (entry.hidden ? 'categoryListTableEditHidden' : 'categoryListTableEdit') : (entry.hidden ? 'categoryListTableHidden' : 'categoryListTable') }">
                <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                <h:panelGroup>
                    <f:verbatim><a name="phase:</f:verbatim>
                    <h:outputText value="#{entry.phaseId}" />
                    <f:verbatim>"></a></f:verbatim>
                    <h:outputText value="#{entry.name}" />
                </h:panelGroup>
                <h:commandButton action="self" alt="#{resources.editIconAlt}"
                        actionListener="#{manager.edit}"
                        rendered="#{userManager.superUser}"
                        image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}">
                    <f:param name="phaseId" value="#{entry.phaseId}" />
                </h:commandButton>
                <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.hideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}" rendered="#{!entry.hidden}">
                    <f:param name="phaseId" value="#{entry.phaseId}" />
                </h:commandButton>
                <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                    <f:param name="phaseId" value="#{entry.phaseId}" />
                </h:commandButton>
                <h:commandButton action="self" alt="#{resources.deleteIconAlt}"
                        actionListener="#{manager.delete}"
                        disabled="#{entry.associated ? true : false}"
                        rendered="#{userManager.superUser}"
                        image="#{facesContext.externalContext.requestContextPath}#{entry.associated ? resources.deleteIconDisabled : resources.deleteIcon}">
                    <f:param name="phaseId" value="#{entry.phaseId}" />
                </h:commandButton>
            </h:panelGrid>
        </f:facet>
    </dmg:tree>

</h:form>
</f:subview>
