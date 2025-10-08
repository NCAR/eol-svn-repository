  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>Create Dataset</title>         
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link action="list">Dataset List</g:link></span>
		    <g:render template="/adminmenubar" /> 
        </div>
        <div class="body">
           <h1>Create Dataset</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${dataset}">
                <div class="errors">
                    <g:renderErrors bean="${dataset}" as="list" />
                </div>
           </g:hasErrors>
           <g:form action="save" method="post" >
               <div class="dialog">
                <table>
                    <tbody>

                                  <tr class='prop'><td valign='top' class='name'><label for='title'>Title:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'title','errors')}'><textarea rows='5' cols='40' name='title'>${dataset?.title?.encodeAsHTML()}</textarea></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='summary'>Summary:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'summary','errors')}'><textarea rows='5' cols='40' name='summary'>${dataset?.summary?.encodeAsHTML()}</textarea></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='beginDate'>Begin Date:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'beginDate','errors')}'><g:datePicker name='beginDate' value="${dataset?.beginDate}" precision='day'></g:datePicker></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='endDate'>End Date:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'endDate','errors')}'><g:datePicker name='endDate' value="${dataset?.endDate}" precision='day'></g:datePicker></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='minimumLatitude'>Minimum Latitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLatitude','errors')}'><input type='text' name='minimumLatitude' value="${dataset?.minimumLatitude}" /></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='maximumLatitude'>Maximum Latitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLatitude','errors')}'><input type='text' name='maximumLatitude' value="${dataset?.maximumLatitude}" /></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='minimumLongitude'>Minimum Longitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLongitude','errors')}'><input type='text' name='minimumLongitude' value="${dataset?.minimumLongitude}" /></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='maximumLongitude'>Maximum Longitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLongitude','errors')}'><input type='text' name='maximumLongitude' value="${dataset?.maximumLongitude}" /></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='piContact'>Pi Contact:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'piContact','errors')}'><g:select optionKey="id" from="${Contact.list()}" name='piContact.id' value="${dataset?.piContact?.id}"></g:select></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='metadataContact'>Metadata Contact:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'metadataContact','errors')}'><g:select optionKey="id" from="${Contact.list()}" name='metadataContact.id' value="${dataset?.metadataContact?.id}"></g:select></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='datacenterContact'>Datacenter Contact:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'datacenterContact','errors')}'><g:select optionKey="id" from="${Contact.list()}" name='datacenterContact.id' value="${dataset?.datacenterContact?.id}"></g:select></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='progress'>Progress:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'progress','errors')}'><g:select name='progress' from='${dataset.constraints.progress.inList.collect{it.encodeAsHTML()}}' value="${dataset.progress?.encodeAsHTML()}"></g:select></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='spatialType'>Spatial Type:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'spatialType','errors')}'><g:select name='spatialType' from='${dataset.constraints.spatialType.inList.collect{it.encodeAsHTML()}}' value="${dataset.spatialType?.encodeAsHTML()}"></g:select></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='locationKeyword'>Location Keyword:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'locationKeyword','errors')}'><g:select optionKey="id" from="${GcmdLocation.list()}" name='locationKeyword.id' value="${dataset?.locationKeyword?.id}"></g:select></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='scienceKeyword'>Science Keyword:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'scienceKeyword','errors')}'><g:select optionKey="id" from="${GcmdScience.list()}" name='scienceKeyword.id' value="${dataset?.scienceKeyword?.id}"></g:select></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='discipline'>Discipline:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'discipline','errors')}'><input type="text" name='discipline' value="${dataset?.discipline?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='nsfAwardNumber'>NSF Award Number:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'nsfAwardNumber','errors')}'><input type='text' name='nsfAwardNumber' value="${dataset?.nsfAwardNumber}" /></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='projectKeyword'>Project Keyword:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'projectKeyword','errors')}'><g:select optionKey="id" from="${GcmdProject.list()}" name='projectKeyword.id' value="${dataset?.projectKeyword?.id}"></g:select></td></tr>
                       
                    </tbody>
               </table>
               </div>
               <div class="buttons">
                     <span class="formButton">
                        <input type="submit" value="Create"></input>
                     </span>
               </div>
            </g:form>
        </div>
    </body>
</html>
