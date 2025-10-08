<%@ taglib prefix="f" uri="http://java.sun.com/jsf/core" %>
<%@ taglib prefix="h" uri="http://java.sun.com/jsf/html" %>

<f:view>
    <f:loadBundle basename="resources" var="resources" />
    <f:loadBundle basename="projectForm" var="projectForm" />
    <f:loadBundle basename="categoryForm" var="categoryForm" />
    <f:loadBundle basename="datasetForm" var="datasetForm" />
    <f:loadBundle basename="help" var="help" />
    <html>
      <head>
        <title>Master List Editor Help</title>
        <link rel="stylesheet" type="text/css" 
           href="<h:outputText value="#{facesContext.externalContext.requestContextPath}/#{resources.css}" />" />
      </head>
      <body>
        <h1 align="center">Master List Editor: Help</h1>
        <hr>
        <h2>Contents</h2>
        <ol>
            <li>
                <h:outputLink value="#button">
                    <h:outputText value="#{help.buttonTitle}" />
                </h:outputLink>
            </li>
            <li>
                <h:outputLink value="#project">
                    <h:outputText value="#{help.projectTitle}" />
                </h:outputLink>
                <h:panelGroup rendered="#{userManager.superUser}">
                    <h:outputText escape="false" value="<ol>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#project:new">
                            <h:outputText value="#{help.projectTitleNew}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#project:edit">
                            <h:outputText value="#{help.projectTitleEdit}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#project:delete">
                            <h:outputText value="#{help.projectTitleDelete}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="</ol>" />
                </h:panelGroup>
            </li>
            <li>
                <h:outputLink value="#category">
                    <h:outputText value="#{help.categoryTitle}" />
                </h:outputLink>
                <h:panelGroup rendered="#{userManager.superUser}">
                    <h:outputText escape="false" value="<ol>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#category:new">
                            <h:outputText value="#{help.categoryTitleNew}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#category:edit">
                            <h:outputText value="#{help.categoryTitleEdit}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#category:delete">
                            <h:outputText value="#{help.categoryTitleDelete}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="</ol>" />
                </h:panelGroup>
            </li>
            <li>
                <h:outputLink value="#dataset">
                    <h:outputText value="#{help.datasetTitle}" />
                </h:outputLink>
                <h:panelGroup>
                    <h:outputText escape="false" value="<ol>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#dataset:new">
                            <h:outputText value="#{help.datasetTitleNew}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#dataset:edit">
                            <h:outputText value="#{help.datasetTitleEdit}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="<li>" />
                        <h:outputLink value="#dataset:delete">
                            <h:outputText value="#{help.datasetTitleDelete}" />
                        </h:outputLink>
                    <h:outputText escape="false" value="</li>" />
                    <h:outputText escape="false" value="</ol>" />
                </h:panelGroup>
            </li>
        </ol>
        <hr>

        <a name="button"></a>
        <h2><h:outputText value="#{help.buttonTitle}" /></h2>
	<dl>
            <dt><h:graphicImage alt="#{resources.todayIconAlt}" value="#{resources.todayIcon}" /></dt>
            <dd>Blue buttons alter the state of the current entry such as sending the user to an editor or filling in default values.</dd>
            <dt><h:graphicImage alt="#{resources.insertIconAlt}" value="#{resources.insertIcon}" /></dt>
            <dd>Green buttons modify the state of the database based on the user's input in the current form such as inserting or updating a data set.</dd>
            <dt><h:graphicImage alt="#{resources.deleteIconAlt}" value="#{resources.deleteIcon}" /></dt>
            <dd>Red buttons alter the state of the database by deleting the current entry.  The user is always prompted to confirm the final deletion.  <span class="warning">Beware that deletions are permanent and cannot be recovered.</span></dd>
            <dt><h:graphicImage alt="#{resources.viewIconAlt}" value="#{resources.viewIcon}" /></dt>
            <dd>Gold buttons take the user out of the Master List Editor to an external page.</dd>
        </dl>
        <h:panelGrid columns="1">
            <h:outputLink value="#">
                <h:outputText value="#{help.topLink}" />
            </h:outputLink>
        </h:panelGrid>
        <hr>
        
        <a name="project"></a>
        <h2><h:outputText value="#{help.projectTitle}" /></h2>
        <p>A <i>project</i> is a set of metadata that consists of a collection of data sets organized into categories.</p>
        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<p>A project has the following fields.</p>" />
            <h:outputText escape="false" value="<dl>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{projectForm.projectId}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The unique identifier for a project." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{projectForm.displayName}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The name of the project to display on the public page.  The project id will be used if this is left empty." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{projectForm.systemDirectory}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The location on the disk where the public pages are stored and accessed." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{projectForm.url}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The URL that accesses the public pages." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{projectForm.homePageUrl}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The URL for the home page for the project used by the Master List menu." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{projectForm.logoUrl}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The URL where the logo for the project can be accessed for the Master List menu." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{projectForm.newLength}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The number of days a data set is to be considered new in the public display." />
                <h:outputText escape="false" value="</dd>" />
            <h:outputText escape="false" value="</dl>" />
        </h:panelGroup>
        <h:panelGrid columns="1">
            <h:outputLink value="#">
                <h:outputText value="#{help.topLink}" />
            </h:outputLink>
        </h:panelGrid>
        <hr>
        
        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<a name=\"project:new\"></a>" />
            <h:outputText escape="false" value="<h3>#{help.projectTitleNew}</h3>" />
            <h:outputText escape="false" value="<p>To create a new project, select \"Add a New Project\" from the menu on the project list page, fill in the fields, associate the categories, and select insert.  The Master List Editor performs multiple checks on the metadata to ensure valid fields.  After the project is created, data sets can be associated through the data set list.</p>" />
            <h:panelGrid columns="1">
                <h:outputLink value="#">
                    <h:outputText value="#{help.topLink}" />
                </h:outputLink>
            </h:panelGrid>
            <h:outputText escape="false" value="<hr>" />
        </h:panelGroup>
        
        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<a name=\"project:edit\"></a>" />
            <h:outputText escape="false" value="<h3>#{help.projectTitleEdit}</h3>" />
            <h:outputText escape="false" value="<p>To edit a project, select the project from the project list or the menu in the data set list, change the value(s), and select update.  If the system directory is changed, it will not remove any old files.  These will need to be removed manually.</p>" />
            <h:panelGrid columns="1">
                <h:outputLink value="#">
                    <h:outputText value="#{help.topLink}" />
                </h:outputLink>
            </h:panelGrid>
            <h:outputText escape="false" value="<hr>" />
        </h:panelGroup>
        
        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<a name=\"project:delete\"></a>" />
            <h:outputText escape="false" value="<h3>#{help.projectTitleDelete}</h3>" />
            <h:outputText escape="false" value="<p>To delete a project, select the project from the project list or select delete from the project editor.  The user will be presented with a confirmation screen to confirm the deletion of the project.  Choosing to confirm the project deletion will remove all project metadata, all data set associations with the project, and any data sets that are only associated with the project.</p>" />
            <h:outputText escape="false" value="<p>This will not remove any of the public files that are in the system directory.  These must be removed manually.</p>" />
            <h:panelGrid columns="1">
                <h:outputLink value="#">
                    <h:outputText value="#{help.topLink}" />
                </h:outputLink>
            </h:panelGrid>
            <h:outputText escape="false" value="<hr>" />
        </h:panelGroup>

        
        <a name="category"></a>
        <h2><h:outputText value="#{help.categoryTitle}" /></h2>
        <p>A <i>category</i> is a piece of metadata used for grouping data sets.</p>
        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<p>A category has the following fields.</p>" />
            <h:outputText escape="false" value="<dl>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{categoryForm.categoryId}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The unique identifier for a category.  This is hidden in the form and is automatically generated by the database." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{categoryForm.name}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText value="The name of the category.  The category name must be unique." />
                <h:outputText escape="false" value="</dd>" />
                <h:outputText escape="false" value="<dt>" />
                    <h:outputText styleClass="formTitle" value="#{categoryForm.parentCategory}" />
                <h:outputText escape="false" value="</dt>" />
                <h:outputText escape="false" value="<dd>" />
                    <h:outputText escape="false" value="The category id of the directory parent of the category.  This value can be <code>null</code> if it does not have a parent." />
                <h:outputText escape="false" value="</dd>" />
            <h:outputText escape="false" value="</dl>" />
        </h:panelGroup>       
        <h:panelGrid columns="1">
            <h:outputLink value="#">
                <h:outputText value="#{help.topLink}" />
            </h:outputLink>
        </h:panelGrid>
        <hr>

        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<a name=\"category:new\"></a>" />
            <h:outputText escape="false" value="<h3>#{help.categoryTitleNew}</h3>" />
            <h:outputText escape="false" value="<p>To create a new category, select \"Add a new Category\" from the menu on the category list page, fill in the fields, and select insert.  After the category is created, it can be associated with projects through the project editor and data sets through the data set editor.</p>" />
            <h:panelGrid columns="1">
                <h:outputLink value="#">
                    <h:outputText value="#{help.topLink}" />
                </h:outputLink>
            </h:panelGrid>
            <h:outputText escape="false" value="<hr>" />
        </h:panelGroup>
        
        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<a name=\"category:edit\"></a>" />
            <h:outputText escape="false" value="<h3>#{help.categoryTitleEdit}</h3>" />
            <h:outputText escape="false" value="<p>To edit a category, select the category from the category list, change the values, and select update.  All projects associated with the category will have their pages regenerated.</p>" />
            <h:panelGrid columns="1">
                <h:outputLink value="#">
                    <h:outputText value="#{help.topLink}" />
                </h:outputLink>
            </h:panelGrid>
            <h:outputText escape="false" value="<hr>" />
        </h:panelGroup>
        
        <h:panelGroup rendered="#{userManager.superUser}">
            <h:outputText escape="false" value="<a name=\"category:delete\"></a>" />
            <h:outputText escape="false" value="<h3>#{help.categoryTitleDelete}</h3>" />
            <h:outputText escape="false" value="<p>A category can be deleted from the category list or from the category editor.  Each will take you to the confirmation screen to confirm that the category is really to be deleted.</p><p>It can only be deleted if it does not have a child category and is not associated with any projects.</p>" />
            <h:panelGrid columns="1">
                <h:outputLink value="#">
                    <h:outputText value="#{help.topLink}" />
                </h:outputLink>
            </h:panelGrid>
            <h:outputText escape="false" value="<hr>" />
        </h:panelGroup>

        
        <a name="dataset"></a>
        <h2><h:outputText value="#{help.datasetTitle}" /></h2>
        <p>A <i>data set</i> is a set of metadata that consists of a set of global metadata, a set of project specific metadata for each project the data set is associated with, and a collection of categories the data set is grouped into.</p>
	<p>A data set has the following fields:</p>
        <dl>
            <dt><h:outputText styleClass="formTitle" 
                    value="#{datasetForm.datasetId}" /></dt>
            <dd>The unique identifier for a data set.</dd>                
            <dt><h:outputText styleClass="formTitle"
                    value="#{datasetForm.name}" /></dt>
            <dd>The name/title of the data set.</dd>
            <dt><h:outputText styleClass="formTitle"
                    value="#{datasetForm.url}" /></dt>
            <dd>The URL that is used to access the data for the data set.</dd>
            <dt><h:outputText styleClass="formTitle"
                    value="#{datasetForm.docUrl}" /></dt>
            <dd>The URL for the primary document for the data set.</dd>
            <dt><h:outputText styleClass="formTitle"
                    value="#{datasetForm.expectedDate}" /></dt>
            <dd>The date the data set is expected to be available.</dd>
            <dt><h:outputText styleClass="formTitle" 
                    value="#{datasetForm.enteredDate}" /></dt>
            <dd>The date to be displayed for the data set.  Often the date the
            data set is entered into the Master List.</dd>
            <dt><h:outputText styleClass="formTitle"
                    value="#{datasetForm.updatedDate}" /></dt>
            <dd>A flag that marks the data set as updated.  This should be set
            if the data for the data set has changed after it was made
            available.</dd>
            <dt><h:outputText styleClass="formTitle"
                    value="#{datasetForm.hidden}" /></dt>
            <dd>A flag that hides a data set for the specified project.</dd>
            <dt><h:outputText styleClass="formTitle"
                    value="#{datasetForm.inProgress}" /></dt>
            <dd>A flag that marks that data set as in progress for the specified
            project.</dt>
        </dl>
        <p>Each data set also has a set of categories that are project
        independant.</p>
                
        <h:panelGrid columns="1">
            <h:outputLink value="#">
                <h:outputText value="#{help.topLink}" />
            </h:outputLink>
        </h:panelGrid>
        <hr>
        
        <a name="dataset:new"></a>
        <h3><h:outputText value="#{help.datasetTitleNew}" /></h3>
        <p>There are two ways to add a new data set for a project, creating a 
        new data set (one that is not in the Master List for any project) or
        associating a data set to the project.</p>
        <h4>A Brand New Data Set</h4>
        <p>To create a brand new data set, select "New Data Set" from the data set list, fill in the known values (both global and project specific), and select insert. If the data set id from the Data Archive System (DAS) is known, use it, otherwise it should be left empty to allow the Master List Editor to generate a Master List specific id for it.  (The auto-generated id should not be used in the DAS.)</p>
        <h4>An Existing Data Set</h4>
        <p>To enter an existing data set, select "New Data Set" from the data set list, enter the data set id, and click on "Fill In" to load the values for the data set into the form.  The data set id must exist in the Master List.  Make the changes to the data set and select update.</p>
        <p class="warning">Changing metadata in the global section will cause 
        changes in the other projects the data set is associated with as well.
        </p>        
        <h:panelGrid columns="1">
            <h:outputLink value="#">
                <h:outputText value="#{help.topLink}" />
            </h:outputLink>
        </h:panelGrid>
        <hr>
        
        <a name="dataset:edit"></a>
        <h3><h:outputText value="#{help.datasetTitleEdit}" /></h3>
        <p>To edit a data set, select the data set to be editted, change the
        values and update it.</p>
        <p class="warning">Changing metadata in the global section will cause 
        changes in the other projects the data set is associated with as well.
        </p>
        <h:panelGrid columns="1">
            <h:outputLink value="#">
                <h:outputText value="#{help.topLink}" />
            </h:outputLink>
        </h:panelGrid>
        <hr>
        
        <a name="dataset:delete"></a>
        <h3><h:outputText value="#{help.datasetTitleDelete}" /></h3>
        <p>A data set can be deleted from the data set list or from the data set edit form.  Each will take you to the confirmation screen to confirm that you want to delete the data set.</p>
        <p>There are two types of data set deletions, deleting a data set that is only associated with the current project or deleting a data set that is associated with multiple projects.  Deleting a data set associated with multiple projects will only remove the metadata unique for the current project.  Deleting a data set that is only association with the current project will remove the entire data set from the Master List.
        </p>
        <h:panelGrid columns="1">
            <h:outputLink value="#">
                <h:outputText value="#{help.topLink}" />
            </h:outputLink>
        </h:panelGrid>
        <hr>
        
        <p>This page was last updated on Sept. 29, 2006</p>
        
      </body>
    </html>


</f:view>