<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>
<%@ taglib prefix="dmg" uri="http://eol.ucar.edu/dmg/taglib" %>

<f:subview id="CategoryListView">
    <f:loadBundle basename="resources" var="Resources" />
    <h:form id="CategoryListForm">

        <dmg:tree depthClasses="categoryListEntry" id="CategoryTree"
                    rootClass="projectTitle"
                    value="#{generalManager.categoryTreeRoot}"
                    var="category" varNodeToggler="t">
            <f:facet name="closedCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName,icon,icon"
                    columns="4" styleClass="#{generalManager.category.categoryId == category.categoryId ? 'categoryListTableEdit' : 'categoryListTable'}"
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.closedTreeIconAlt}" value="#{resources.closedTreeIcon}" />
                    </h:commandLink>
                    <h:panelGroup>
                        <f:verbatim><a name="category:</f:verbatim>
                        <h:outputText value="#{category.categoryId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                    <h:commandButton action="self" alt="#{resources.editIconAlt}"
                            actionListener="#{generalManager.edit}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}">
                        <f:param name="categoryId" value="#{category.categoryId}" />
                    </h:commandButton>
                    <h:commandButton action="self" alt="#{resources.deleteIconAlt}"
                            disabled="true"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.deleteIconDisabled}">
                        <f:param name="categoryId" value="#{category.categoryId}" />
                    </h:commandButton>
                </h:panelGrid>
            </f:facet>
            <f:facet name="leafCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName,icon,icon" 
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);"
                    columns="4" styleClass="#{generalManager.category.categoryId == category.categoryId ? 'categoryListTableEdit' : 'categoryListTable'}">
                    <h:graphicImage alt="#{resources.leafTreeIconAlt}" value="#{resources.leafTreeIcon}" />
                    <h:panelGroup>
                        <f:verbatim><a name="category:</f:verbatim>
                        <h:outputText value="#{category.categoryId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                    <h:commandButton action="self" alt="#{resources.editIconAlt}"
                            actionListener="#{generalManager.edit}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}">
                        <f:param name="categoryId" value="#{category.categoryId}" />
                    </h:commandButton>
                    <h:commandButton action="self" alt="#{resources.deleteIconAlt}"
		            actionListener="#{generalManager.delete}"
                            disabled="#{category.associated ? true : false}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{category.associated ? resources.deleteIconDisabled : resources.deleteIcon}">
                        <f:param name="categoryId" value="#{category.categoryId}" />
                    </h:commandButton>
                </h:panelGrid>
            </f:facet>
            <f:facet name="openCategory">
                <h:panelGrid columnClasses="categoryTreeIcon,categoryName,icon,icon" 
                    onmouseover="hoverCategory(this,true);"
                    onmouseout="hoverCategory(this,false);"
                    columns="4" styleClass="#{generalManager.category.categoryId == category.categoryId ? 'categoryListTableEdit' : 'categoryListTable'}">
                    <h:commandLink action="self" actionListener="#{t.toggle}">
                        <h:graphicImage alt="#{resources.openTreeIconAlt}" value="#{resources.openTreeIcon}" />
                    </h:commandLink>
                    <h:panelGroup>
                        <f:verbatim><a name="category:</f:verbatim>
                        <h:outputText value="#{category.categoryId}" />
                        <f:verbatim>"></a></f:verbatim>
                        <h:outputText value="#{category.name}" />
                    </h:panelGroup>
                    <h:commandButton action="self" alt="#{resources.editIconAlt}"
                            actionListener="#{generalManager.edit}"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.editIcon}">
                        <f:param name="categoryId" value="#{category.categoryId}" />
                    </h:commandButton>
                    <h:commandButton action="self" alt="#{resources.deleteIconAlt}"
                            disabled="true"
                            rendered="#{userManager.superUser}"
                            image="#{facesContext.externalContext.requestContextPath}#{resources.deleteIconDisabled}">
                        <f:param name="categoryId" value="#{category.categoryId}" />
                    </h:commandButton>
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