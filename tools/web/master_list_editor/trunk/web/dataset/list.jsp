<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="dmg" uri="http://eol.ucar.edu/dmg/taglib" %>

<f:subview id="DatasetListView">
    <f:loadBundle basename="resources" var="Resources" />
    <h:form id="DatasetListForm">
        
        <dmg:datasetList depthClasses="projectChild" id="DatasetList" 
                        rootClass="projectTitle" 
                        value="#{manager.datasetListRoot}"
                        var="entry" varNodeToggler="t">
            <f:facet name="leafPhase">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:graphicImage alt="#{Resources.leafTreeIconAlt}" value="#{Resources.leafTreeIcon}" />
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup>
                        <h:commandButton action="self" 
                                actionListener="#{manager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="phaseId" value="#{entry.phaseId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="phaseId" value="#{entry.phaseId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="closedPhase">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{Resources.closedTreeIconAlt}" value="#{Resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup>
                        <h:commandButton action="self" 
                                actionListener="#{manager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="phaseId" value="#{entry.phaseId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="phaseId" value="#{entry.phaseId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openPhase">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{Resources.openTreeIconAlt}" value="#{Resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup style="vertical-align: middle;">
                        <h:commandButton action="self" 
                                actionListener="#{manager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="phaseId" value="#{entry.phaseId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="phaseId" value="#{entry.phaseId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="closedClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{Resources.closedTreeIconAlt}" value="#{Resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup>
                        <h:commandButton action="self" 
                                actionListener="#{manager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="classificationId" value="#{entry.classificationId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="classificationId" value="#{entry.classificationId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:graphicImage alt="#{Resources.leafTreeIconAlt}" value="#{Resources.leafTreeIcon}" />
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup>
                        <h:commandButton action="self" 
                                actionListener="#{manager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="classificationId" value="#{entry.classificationId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="classificationId" value="#{entry.classificationId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,datasetListCategory,icon" columns="3">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{Resources.openTreeIconAlt}" value="#{Resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:outputText value="#{entry.name}" />
                    <h:panelGroup style="vertical-align: middle;">
                        <h:commandButton action="self" 
                                actionListener="#{manager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="classificationId" value="#{entry.classificationId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="classificationId" value="#{entry.classificationId}" />
                        </h:commandButton>
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openClassType">
                <h:outputText value="#{entry.name}" />
            </f:facet>
            <f:facet name="leafDataset">
                <h:panelGrid columnClasses="editIconCol,hideIconCol,deleteIconCol,datasetIdCol,datasetNameCol,datasetAuthorCol,datasetStatusCol,datasetDocCol" columns="8" 
                            onmouseout="hoverDataset(this,false);"
                            onmouseover="hoverDataset(this,true);"
                            styleClass="#{manager.dataset.datasetId == entry.datasetId ? (entry.hidden ? 'datasetEntryEditHidden' : 'datasetEntryEdit') : 'datasetEntryRow'}">
                    <h:commandButton action="self" actionListener="#{manager.edit}" alt="#{Resources.editIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.editIcon}">
                        <f:param name="datasetId" value="#{entry.datasetId}" />
                    </h:commandButton>
                    <h:panelGroup>
                        <f:verbatim><a name="dataset:</f:verbatim>
                            <h:outputText value="#{entry.datasetId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:commandButton action="self" 
                                actionListener="#{manager.hide}"
                                alt="#{Resources.hideIconAlt}"
                                image="#{facesContext.externalContext.requestContextPath}/#{Resources.hideIcon}"
                                rendered="#{!entry.hidden}">
                            <f:param name="datasetId" value="#{entry.datasetId}" />
                        </h:commandButton>
                        <h:commandButton action="self" actionListener="#{manager.hide}" alt="#{Resources.unhideIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.unhideIcon}" rendered="#{entry.hidden}">
                            <f:param name="datasetId" value="#{entry.datasetId}" />
                        </h:commandButton>
                    </h:panelGroup>
                    <h:commandButton action="self" actionListener="#{manager.delete}" alt="#{Resources.deleteIconAlt}" image="#{facesContext.externalContext.requestContextPath}/#{Resources.deleteIcon}">
                        <f:param name="datasetId" value="#{entry.datasetId}" />
                    </h:commandButton>
                    <h:outputText value="#{entry.datasetId}" />
                    <h:panelGroup>
                        <h:outputLink rendered="#{entry.url != ''}" value="#{entry.url}">
                            <h:outputText value="#{entry.name}" />
                        </h:outputLink>
                        <h:outputText rendered="#{entry.url == ''}" value="#{entry.name}" />
                    </h:panelGroup>
                    <h:outputText value="#{entry.authorPi}" />
                    <h:panelGroup>
                        <h:panelGrid columns="1" styleClass="#{entry.preliminary && entry.dateExpected != null && (entry.inProgress || entry.updated || entry.new || entry.datePosted != null) ? 'prelimBox' : ''}">
                            <h:graphicImage alt="#{Resources.preliminaryIconAlt}" rendered="#{entry.preliminary}" styleClass="preliminary" value="#{Resources.preliminaryIcon}" />
                            <h:graphicImage alt="#{Resources.inProgressIconAlt}" rendered="#{entry.inProgress}" styleClass="inProgress" value="#{Resources.inProgressIcon}" />
                            <h:graphicImage alt="#{Resources.updatedIconAlt}" rendered="#{entry.updated && !entry.inProgress}" styleClass="updated" value="#{Resources.updatedIcon}" />
                            <h:outputText rendered="#{entry.updated && !entry.inProgress}" styleClass="updated" value="#{entry.dateUpdated}" />
                            <h:graphicImage alt="#{Resources.newIconAlt}" rendered="#{entry.new}" styleClass="new" value="#{Resources.newIcon}" />
                            <h:outputText rendered="#{entry.datePosted != null && !entry.inProgress && !entry.updated}" styleClass="#{entry.new ? 'new' : ''}" value="#{entry.datePosted}" />
                        </h:panelGrid>
                        <h:panelGrid columns="1" rendered="#{entry.preliminary && entry.dateExpected != null && (entry.inProgress || entry.updated || entry.datePosted != null)}" styleClass="prelimBox">
                            <h:graphicImage alt="#{Resources.updateExpectedIconAlt}" rendered="#{entry.dateExpected != null  && (entry.preliminary || (!entry.inProgress && !entry.updated && !entry.new))}" styleClass="expected" value="#{Resources.updateExpectedIcon}" />
                            <h:outputText rendered="#{entry.dateExpected != null && (entry.preliminary || (!entry.inProgress && !entry.updated && !entry.new))}" styleClass="expected" value="#{entry.dateExpected}" />
                        </h:panelGrid>
                        <h:panelGrid columns="1" rendered="#{entry.dateExpected != null && (!entry.preliminary || (entry.preliminary && !entry.inProgress && !entry.updated && entry.datePosted == null))}">
                            <h:graphicImage alt="#{Resources.expectedIconAlt}" rendered="#{entry.dateExpected != null  && (entry.preliminary || (!entry.inProgress && !entry.updated && !entry.new))}" styleClass="expected" value="#{Resources.expectedIcon}" />
                            <h:outputText rendered="#{entry.dateExpected != null && (entry.preliminary || (!entry.inProgress && !entry.updated && !entry.new))}" styleClass="expected" value="#{entry.dateExpected}" />
                        </h:panelGrid>
                    </h:panelGroup>
                    <h:panelGroup>
                        <h:outputLink rendered="#{entry.docUrl != ''}" value="#{entry.docUrl}" rel="nofollow" target="_blank">
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
