<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="dmg" uri="http://eol.ucar.edu/dmg/taglib" %>

<f:subview id="MenuSubview">
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="MenuForm">
        <f:verbatim><div id="menu"></f:verbatim>

        
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{manager.viewProjectList}"
                    rendered="#{manager.listState}"
                    value="#{resources.projectTitle}" />
            <h:outputText rendered="#{!manager.listState}"
                    value="#{resources.projectTitle}" />
        <f:verbatim></div></f:verbatim>
        
        <h:panelGroup rendered="#{userManager.superUser && manager.projectState && manager.listState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{manager.edit}"
                    value="#{resources.newProjectTitle}" />
            <f:verbatim></div></f:verbatim>
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:outputText rendered="false"
                    value="#{resources.cloneProjectTitle}" />
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>
        
        <h:panelGroup rendered="#{manager.datasetState && manager.listState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:outputLink value="#{manager.project.url}">
                <h:outputText value="#{resources.viewProjectTitle}" />
            </h:outputLink>
            <f:verbatim></div></f:verbatim>

            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{manager.edit}"
                    rendered="#{userManager.superUser}"
                    value="#{resources.editProjectTitle}">
                <f:param name="projectId" 
                        value="#{manager.project.projectId}" />
            </h:commandLink>
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>        

        
       
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{manager.viewClassificationList}"
                    rendered="#{manager.listState}"
                    value="#{resources.categoryTitle}" />
            <h:outputText rendered="#{!manager.listState}"
                    value="#{resources.categoryTitle}" />
        <f:verbatim></div></f:verbatim>
        
        <h:panelGroup rendered="#{userManager.superUser && manager.classificationState && manager.listState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{manager.edit}"
                    value="#{resources.newCategoryTitle}" />
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>


        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{manager.viewPhaseList}"
                    rendered="#{manager.listState}"
                    value="#{resources.phaseTitle}" />
            <h:outputText rendered="#{!manager.listState}"
                    value="#{resources.phaseTitle}" />
        <f:verbatim></div></f:verbatim>
        
        <h:panelGroup rendered="#{userManager.superUser && manager.phaseState && manager.listState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{manager.edit}"
                    value="#{resources.newPhaseTitle}" />
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>
        
        
        
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{manager.viewDatasetList}"
                    rendered="false"
                    value="#{resources.datasetTitle}" />
            <h:outputText rendered="true"
                    value="#{resources.datasetTitle}" />
        <f:verbatim></div></f:verbatim>

        <h:panelGroup rendered="#{manager.datasetState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{manager.edit}"
                    rendered="#{manager.listState}"
                    value="#{resources.datasetNewTitle}" />
            <h:outputText rendered="#{!manager.listState}"
                    value="#{resources.datasetNewTitle}" />
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>        
        
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{manager.viewDatasetList}"
                    rendered="#{manager.datasetState &&
                                manager.listState}"
                    value="#{resources.viewsTitle}" />
            <h:outputText rendered="#{!(manager.datasetState &&
                                manager.listState)}"
                    value="#{resources.viewsTitle}" />
        <f:verbatim></div></f:verbatim>
        

        <dmg:tree depthClasses="categoryMenuEntry" id="ViewPhaseTree" rendered="#{manager.datasetState && manager.phaseTreeRoot != null}" rootClass="classificationTypeEntry" value="#{manager.phaseTreeRoot}" var="phase" varNodeToggler="p">
            <f:facet name="leafPhase">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{phase.name}" />
                        <f:param name="phaseId" value="#{phase.phaseId}" />                        
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openPhaseRoot">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{p.toggle}">
                        <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{resources.phaseTitle}" />
                        <f:param name="phaseId" value="0" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="closedPhaseRoot">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{p.toggle}">
                        <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{resources.phaseTitle}" />
                        <f:param name="phaseId" value="0" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
        </dmg:tree>
        
        <dmg:tree depthClasses="classificationTypeEntry,categoryMenuEntry,categorySubMenuEntry" id="ViewTree" rendered="#{manager.datasetState}" value="#{manager.classificationTreeRoot}" var="category" varNodeToggler="t">
            <f:facet name="closedClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="classificationId" value="#{category.classificationId}" />
                        <f:param name="projectId" value="#{manager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="closedClassType">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="typeId" value="#{category.typeId}" />
                        <f:param name="projectId" value="#{manager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="classificationId" value="#{category.classificationId}" />
                        <f:param name="projectId" value="#{manager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafClassType">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="typeId" value="#{category.typeId}" />
                        <f:param name="projectId" value="#{manager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="classificationId" value="#{category.classificationId}" />
                        <f:param name="projectId" value="#{manager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openClassType">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{manager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="typeId" value="#{category.typeId}" />
                        <f:param name="projectId" value="#{manager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafProject">
                <h:panelGroup>
                    <f:verbatim><div></f:verbatim>
                    <h:outputText value=""/>
                    <%-- Don't display anything! --%>
                    <f:verbatim></div></f:verbatim>
                </h:panelGroup>
            </f:facet>
            <f:facet name="openProject">
                <h:panelGroup>
                    <f:verbatim><div></f:verbatim>
                    <h:outputText value=""/>
                    <%-- Don't display anything! --%>
                    <f:verbatim></div></f:verbatim>
                </h:panelGroup>
            </f:facet>
        </dmg:tree>
        <f:verbatim></div></f:verbatim>
        
        <f:verbatim><div id="menufooter"></f:verbatim>
            <h:panelGrid columnClasses="icon" columns="2" styleClass="centered">
                <h:commandButton action="self"
                        alt="#{resources.helpIconAlt}"
                        image="#{facesContext.externalContext.requestContextPath}/#{resources.helpIcon}"
                        onclick="openHelpWindow('/master_list/docs/help/index.jsf')" />
                <h:commandButton action="self"
                        actionListener="#{userManager.logout}"
                        alt="#{resources.logoutIconAlt}"
                        image="#{facesContext.externalContext.requestContextPath}/#{resources.logoutIcon}" />
            </h:panelGrid>
        <f:verbatim></div></f:verbatim>
    </h:form>
</f:subview>
