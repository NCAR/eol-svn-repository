<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="dmg" uri="http://eol.ucar.edu/dmg/taglib" %>

<f:subview id="DatasetListView">
    <f:loadBundle basename="resources" var="Resources" />
    <h:form id="DatasetListForm">
        
        <dmg:datasetList depthClasses="projectChild" id="DatasetList" 
                        rootClass="projectTitle" 
                        value="#{generalManager.datasetListRoot}"
                        var="entry" varNodeToggler="t">
            <f:facet name="closedCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{Resources.closedTreeIconAlt}" value="#{Resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup>
                        <h:commandButton action="self" 
                                actionListener="#{generalManager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="categoryId" value="#{entry.categoryId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{generalManager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="categoryId" value="#{entry.categoryId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:graphicImage alt="#{Resources.leafTreeIconAlt}" value="#{Resources.leafTreeIcon}" />
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup>
                        <h:commandButton action="self" 
                                actionListener="#{generalManager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="categoryId" value="#{entry.categoryId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{generalManager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="categoryId" value="#{entry.categoryId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{Resources.openTreeIconAlt}" value="#{Resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup style="vertical-align: middle;">
                        <h:commandButton action="self" 
                                actionListener="#{generalManager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="categoryId" value="#{entry.categoryId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{generalManager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="categoryId" value="#{entry.categoryId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafDataset">
                <h:panelGrid columnClasses="editIconCol,hideIconCol,deleteIconCol,datasetIdCol,datasetNameCol,datasetStatusCol,datasetDocCol" columns="7" 
                            onmouseout="hoverDataset(this,false);"
                            onmouseover="hoverDataset(this,true);"
                            styleClass="#{generalManager.dataset.datasetId == entry.datasetId ? (entry.hidden ? 'datasetEntryEditHidden' : 'datasetEntryEdit') : 'datasetEntryRow'}">
                    <h:commandButton action="self" actionListener="#{generalManager.edit}" alt="#{Resources.editIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.editIcon}">
                        <f:param name="datasetId" value="#{entry.datasetId}" />
                    </h:commandButton>
                    <h:panelGroup>
                        <f:verbatim><a name="dataset:</f:verbatim>
                            <h:outputText value="#{entry.datasetId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:commandButton action="self" 
                                actionListener="#{generalManager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="datasetId" value="#{entry.datasetId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{generalManager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="datasetId" value="#{entry.datasetId}" />
                        </h:commandButton>
                    </h:panelGroup>
                    <h:commandButton action="self" actionListener="#{generalManager.delete}" alt="#{Resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.deleteIcon}">
                        <f:param name="datasetId" value="#{entry.datasetId}" />
                    </h:commandButton>
                    <h:outputText value="#{entry.datasetId}" />
                    <h:panelGroup>
                        <h:outputLink rendered="#{entry.url != ''}" value="#{entry.url}">
                            <h:outputText value="#{entry.name}" />
                        </h:outputLink>
                        <h:outputText rendered="#{entry.url == ''}" value="#{entry.name}" />
                    </h:panelGroup>
                    <h:panelGroup>
                        <h:panelGrid columns="1" styleClass="#{entry.inProgress ? 'inProgress' : entry.expected ? 'expected' : entry.updated ? 'updated' : entry.new ? 'new' : ''}" >
                            <h:graphicImage alt="#{Resources.expectedIconAlt}" rendered="#{entry.expected}" styleClass="expected" value="#{Resources.expectedIcon}" />
                            <h:graphicImage alt="#{Resources.inProgressIconAlt}" rendered="#{entry.inProgress}" styleClass="inProgress" value="#{Resources.inProgressIcon}" />
                            <h:graphicImage alt="#{Resources.updatedIconAlt}" rendered="#{entry.updated && !entry.inProgress}" styleClass="updated" value="#{Resources.updatedIcon}" />
                            <h:graphicImage alt="#{Resources.newIconAlt}" rendered="#{entry.new}" styleClass="new" value="#{Resources.newIcon}" />
                            <h:outputText rendered="#{!entry.expected && !entry.inProgress && !entry.updated}" value="#{entry.datePosted}" />
                            <h:outputText rendered="#{entry.expected && !entry.inProgress && !entry.updated}" value="#{entry.dateExpected}" />
                            <h:outputText rendered="#{!entry.expected && !entry.inProgress && entry.updated}" value="#{entry.dateUpdated}" />
                        </h:panelGrid>
                    </h:panelGroup>
                    <h:panelGroup>
                        <h:outputLink rendered="#{entry.docUrl != ''}" value="#{entry.docUrl}">
                            <h:graphicImage alt="#{Resources.docIconAlt}" value="#{Resources.docIcon}" />
                        </h:outputLink>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafProject">
                <h:panelGroup>
                    <h:outputText value="#{entry.projectId} #{Resources.datasetTitle}" />
                </h:panelGroup>
            </f:facet>
            <f:facet name="openProject">
                <h:panelGroup>
                    <h:outputText value="#{entry.projectId} #{Resources.datasetTitle}" />
                </h:panelGroup>
            </f:facet>
        </dmg:datasetList>

    </h:form>
</f:subview>