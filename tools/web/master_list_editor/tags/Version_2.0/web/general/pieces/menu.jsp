<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="dmg" uri="http://eol.ucar.edu/dmg/taglib" %>

<f:subview id="MenuSubview">
    <f:loadBundle basename="resources" var="resources" />
    
    <h:form id="MenuForm">
        <f:verbatim><div id="menu"></f:verbatim>

        
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{generalManager.viewProjectList}"
                    rendered="#{generalManager.listState}"
                    value="#{resources.projectTitle}" />
            <h:outputText rendered="#{!generalManager.listState}"
                    value="#{resources.projectTitle}" />
        <f:verbatim></div></f:verbatim>
        
        <h:panelGroup rendered="#{userManager.superUser && generalManager.projectState && generalManager.listState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{generalManager.edit}"
                    value="#{resources.newProjectTitle}" />
            <f:verbatim></div></f:verbatim>
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:outputText rendered="false"
                    value="#{resources.cloneProjectTitle}" />
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>
        
        <h:panelGroup rendered="#{generalManager.datasetState && generalManager.listState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:outputLink value="#{generalManager.project.url}">
                <h:outputText value="#{resources.viewProjectTitle}" />
            </h:outputLink>
            <f:verbatim></div></f:verbatim>

            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{generalManager.edit}"
                    rendered="#{userManager.superUser}"
                    value="#{resources.editProjectTitle}">
                <f:param name="projectId" 
                        value="#{generalManager.project.projectId}" />
            </h:commandLink>
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>        

        
        
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{generalManager.viewCategoryList}"
                    rendered="#{generalManager.listState}"
                    value="#{resources.categoryTitle}" />
            <h:outputText rendered="#{!generalManager.listState}"
                    value="#{resources.categoryTitle}" />
        <f:verbatim></div></f:verbatim>
        
        <h:panelGroup rendered="#{userManager.superUser && generalManager.categoryState && generalManager.listState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{generalManager.edit}"
                    value="#{resources.newCategoryTitle}" />
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>

        
        
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self" 
                    actionListener="#{generalManager.viewDatasetList}"
                    rendered="false"
                    value="#{resources.datasetTitle}" />
            <h:outputText rendered="true"
                    value="#{resources.datasetTitle}" />
        <f:verbatim></div></f:verbatim>

        <h:panelGroup rendered="#{generalManager.datasetState}">
            <f:verbatim><div class="menuEntry"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{generalManager.edit}"
                    rendered="#{generalManager.listState}"
                    value="#{resources.datasetNewTitle}" />
            <h:outputText rendered="#{!generalManager.listState}"
                    value="#{resources.datasetNewTitle}" />
            <f:verbatim></div></f:verbatim>
        </h:panelGroup>        
        
        <f:verbatim><div class="menuHead"></f:verbatim>
            <h:commandLink action="self"
                    actionListener="#{generalManager.viewDatasetList}"
                    rendered="#{generalManager.datasetState &&
                                generalManager.listState}"
                    value="#{resources.viewsTitle}" />
            <h:outputText rendered="#{!(generalManager.datasetState &&
                                generalManager.listState)}"
                    value="#{resources.viewsTitle}" />
        <f:verbatim></div></f:verbatim>
        
        
        <dmg:tree depthClasses="categoryMenuEntry,categorySubMenuEntry" id="ViewTree" rendered="#{generalManager.datasetState}" value="#{generalManager.categoryTreeRoot}" var="category" varNodeToggler="t">
            <f:facet name="closedCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{generalManager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="categoryId" value="#{category.categoryId}" />
                        <f:param name="projectId" value="#{generalManager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                    <h:commandLink action="self" actionListener="#{generalManager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="categoryId" value="#{category.categoryId}" />
                        <f:param name="projectId" value="#{generalManager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryTreeLabel" columns="2">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:commandLink action="self" actionListener="#{generalManager.viewDatasetList}">
                        <h:outputText value="#{category.name}" />
                        <f:param name="categoryId" value="#{category.categoryId}" />
                        <f:param name="projectId" value="#{generalManager.project.projectId}" />
                    </h:commandLink>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openProject">
                <h:panelGroup>
                    <%-- Don't display anything! --%>
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
