<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="dmg" uri="http://eol.ucar.edu/dmg/taglib" %>

<f:subview id="CategoryListView">
    <f:loadBundle basename="resources" var="Resources" />
    <h:form id="CategoryListForm">

        <dmg:tree depthClasses="categoryListEntry" id="CategoryTree"
                    rootClass="projectTitle"
                    value="#{manager.classificationTreeRoot}"
                    var="category" varNodeToggler="t">
            <f:facet name="closedClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName,icon,icon"
                    columns="4" styleClass="#{manager.classification.classificationId == category.classificationId ? 'categoryListTableEdit' : 'categoryListTable'}"
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:panelGroup>
                        <f:verbatim><a name="category:</f:verbatim>
                        <h:outputText value="#{category.classificationId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                    <h:commandButton action="self" alt="#{resources.editIconAlt}"
                            actionListener="#{manager.edit}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}">
                        <f:param name="classificationId" value="#{category.classificationId}" />
                    </h:commandButton>
                    <h:commandButton action="self" alt="#{resources.deleteIconAlt}"
                            disabled="true"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.deleteIconDisabled}">
                        <f:param name="classificationId" value="#{category.classificationId}" />
                    </h:commandButton>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName,icon,icon" 
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);"
                    columns="4" styleClass="#{manager.classification.classificationId == category.classificationId ? 'categoryListTableEdit' : 'categoryListTable'}">
                    <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                    <h:panelGroup>
                        <f:verbatim><a name="category:</f:verbatim>
                        <h:outputText value="#{category.classificationId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                    <h:commandButton action="self" alt="#{resources.editIconAlt}"
                            actionListener="#{manager.edit}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}">
                        <f:param name="classificationId" value="#{category.classificationId}" />
                    </h:commandButton>
                    <h:commandButton action="self" alt="#{resources.deleteIconAlt}"
		            actionListener="#{manager.delete}"
                            disabled="#{category.associated ? true : false}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{category.associated ? resources.deleteIconDisabled : resources.deleteIcon}">
                        <f:param name="classificationId" value="#{category.classificationId}" />
                    </h:commandButton>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openClassification">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName,icon,icon" 
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);"
                    columns="4" styleClass="#{manager.classification.classificationId == category.classificationId ? 'categoryListTableEdit' : 'categoryListTable'}">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:panelGroup>
                        <f:verbatim><a name="category:</f:verbatim>
                        <h:outputText value="#{category.classificationId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                    <h:commandButton action="self" alt="#{resources.editIconAlt}"
                            actionListener="#{manager.edit}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}">
                        <f:param name="classificationId" value="#{category.classificationId}" />
                    </h:commandButton>
                    <h:commandButton action="self" alt="#{resources.deleteIconAlt}"
                            disabled="true"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.deleteIconDisabled}">
                        <f:param name="classificationId" value="#{category.classificationId}" />
                    </h:commandButton>
                </h:panelGrid>
            </f:facet>
            <f:facet name="closedClassType">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName" 
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);"
                    columns="2" styleClass="#{manager.classification.classificationId == category.typeId ? 'categoryListTableEdit' : 'categoryListTable'}">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:panelGroup>
                        <f:verbatim><a name="type:</f:verbatim>
                        <h:outputText value="#{category.typeId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafClassType">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName" 
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);"
                    columns="2" styleClass="#{manager.classification.classificationId == category.typeId ? 'categoryListTableEdit' : 'categoryListTable'}">
                    <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                    <h:panelGroup>
                        <f:verbatim><a name="type:</f:verbatim>
                        <h:outputText value="#{category.typeId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openClassType">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName" 
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);"
                    columns="2" styleClass="#{manager.classification.classificationId == category.typeId ? 'categoryListTableEdit' : 'categoryListTable'}">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:panelGroup>
                        <f:verbatim><a name="type:</f:verbatim>
                        <h:outputText value="#{category.typeId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openProject">
                <h:panelGroup>
                    <h:outputText value="#{resources.categoryTitle}" />
                </h:panelGroup>
            </f:facet>
        </dmg:tree>

    </h:form>
</f:subview>