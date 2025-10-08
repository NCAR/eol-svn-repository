<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:subview id="ProjectListView">
    <h:form id="ProjectListForm">
        <f:loadBundle basename="resources" var="resources" />

        <f:verbatim><h1 class="mainTitle"></f:verbatim>
            <h:outputText value="#{resources.projectTitle}" />
        <f:verbatim></h1></f:verbatim>

        <h:dataTable styleClass="projectTable" value="#{manager.projectList}" var="project">
            <h:column>
                <h:panelGrid columnClasses="projectName,icon,icon,icon,icon"
                        columns="5"
                        onmouseout="this.className='projectTableEntry'"
                        onmouseover="this.className='projectTableEntryHover'"
                        styleClass="projectTableEntry">
                    <h:panelGroup>
                        <f:verbatim><a name="project:</f:verbatim>
                            <h:outputText value="#{project.projectId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:commandLink action="self" 
                                actionListener="#{manager.viewDatasetList}"
                                value="#{project.projectId}">
                            <f:param name="projectId" value="#{project.projectId}"/>
                        </h:commandLink>
                    </h:panelGroup>
                    <h:outputLink value="#{project.url}">
                        <h:graphicImage alt="#{resources.viewIconAlt}"
                                value="#{resources.viewIcon}" />
                    </h:outputLink>
                    <h:commandButton action="self"
                            actionListener="#{manager.edit}"
                            alt="#{resources.editIconAlt}"
image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}"
                            rendered="#{userManager.superUser}">
                        <f:param name="projectId" value="#{project.projectId}"/>
                    </h:commandButton>
                    <h:commandButton action="self"
                            actionListener="#{manager.delete}"
                            alt="#{resources.deleteIconAlt}"
 image="#{facesContext.externalContext.requestContextPath}#{resources.deleteIcon}"
                            rendered="#{userManager.superUser}">
                        <f:param name="projectId" value="#{project.projectId}"/>
                    </h:commandButton>
                    <h:panelGroup rendered="false">
                        <h:commandButton action="self"
                                actionListener="#{manager.hide}"
                                alt="#{resources.hideIconAlt}"
 image="#{facesContext.externalContext.requestContextPath}/#{resources.hideIcon}"
                                rendered="#{!project.hidden}">
                            <f:param name="projectId"
                                    value="#{project.projectId}" />
                        </h:commandButton>
                        <h:commandButton action="self"
                                actionListener="#{manager.hide}"
                                alt="#{resources.unhideIconAlt}"
 image="#{facesContext.externalContext.requestContextPath}/#{resources.unhideIcon}"
                                rendered="#{project.hidden}">
                            <f:param name="projectId"
                                    value="#{project.projectId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </h:column>
        </h:dataTable>
    </h:form>
</f:subview>
