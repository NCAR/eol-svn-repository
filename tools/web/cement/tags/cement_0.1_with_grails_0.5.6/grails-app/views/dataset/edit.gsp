<html>
    <head>
         <title>Edit Dataset</title>
         <meta name="layout" content="formpage" />
		 <g:javascript library="cement" />
    </head>

    <body>
        <div class="body">
           <h1>Edit Dataset Metadata</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${dataset}">
                <div class="errors">
                    <g:renderErrors bean="${dataset}" as="list" />
                </div>
           </g:hasErrors>
           <g:form controller="dataset" method="post" >
               <input type="hidden" name="id" value="${dataset?.id}" />
               <div class="dialog">
                <table>
                    <tbody>
                       
				<tr class='prop'><td valign='top' class='name'><label for='title'>Title:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'title','errors')}'><input type='text' name='title' maxlength='255' size='80' value="${dataset?.title?.encodeAsHTML()}" /></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='summary'>Summary:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'summary','errors')}'><textarea style='height:auto; width:auto;' rows='16' cols='72' name='summary'>${dataset?.summary?.encodeAsHTML()}</textarea></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='beginDate'>Begin Date:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'beginDate','errors')}'><g:datePicker name='beginDate' value="${dataset?.beginDate}" precision='day'></g:datePicker></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='endDate'>End Date:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'endDate','errors')}'><g:datePicker name='endDate' value="${dataset?.endDate}" precision='day'></g:datePicker></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='minimumLatitude'>Minimum Latitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLatitude','errors')}'><input type='text' name='minimumLatitude' value="${dataset?.minimumLatitude}" /></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='maximumLatitude'>Maximum Latitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLatitude','errors')}'><input type='text' name='maximumLatitude' value="${dataset?.maximumLatitude}" /></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='minimumLongitude'>Minimum Longitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'minimumLongitude','errors')}'><input type='text' name='minimumLongitude' value="${dataset?.minimumLongitude}" /></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='maximumLongitude'>Maximum Longitude:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'maximumLongitude','errors')}'><input type='text' name='maximumLongitude' value="${dataset?.maximumLongitude}" /></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='piContact'>Pi Contact:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'piContact','errors')}'><g:select optionKey="id" from="${Contact.list()}" name='piContact.id' value="${dataset?.piContact?.id}"></g:select></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='metadataContact'>Metadata Contact:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'metadataContact','errors')}'><g:select optionKey="id" from="${Contact.list()}" name='metadataContact.id' value="${dataset?.metadataContact?.id}"></g:select></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='datacenterContact'>Datacenter Contact:</label></td><td valign='top' class='value ${hasErrors(bean:dataset,field:'datacenterContact','errors')}'><g:select optionKey="id" from='${[""] + Contact.list()}' name='datacenterContact.id' value="${dataset?.datacenterContact?.id}"></g:select></td></tr>
                       
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
                     <span class="button"><g:actionSubmit value="Update" /></span>
                 	 <span class="button"><g:actionSubmit value="Delete" onclick="return warnBeforeDatasetDelete();" /></span>
               </div>
            </g:form>
        </div>
    <td height="808" width="22%" valign="top">
      <table width="100%" border="0" cellspacing="2" cellpadding="2" height="638">
        <tr>
          <td height="74" valign="top" align="left"><img src="${createLinkTo(dir:'images',file:'cadis_logo.gif')}" alt="CADIS" width="217" height="84" align="top" /></td>
        </tr>
        <tr>
          <td height="388" valign="top" align="left">
            <p>&nbsp;</p> 
            <p>&nbsp;</p>
            <p>&nbsp;</p>
            <p><img src="${createLinkTo(dir:'images',file:'1_nav_panel.gif')}" alt="123" width="148" height="182" align="absmiddle" /></p>
          </td>
        </tr>
        <tr>
          <td height="119" valign="top" align="left"> <a href="../.."><img src="${createLinkTo(dir:'images/buttons',file:'back.gif')}"></a> 
																		<a href="../../contact/show/149"><img src="${createLinkTo(dir:'images/buttons',file:'next.gif',file:'next.gif')}"></a> 
        </tr>
      </table>
      <p>&nbsp;</p>
      <p>&nbsp;</p>
      </td>
    </body>
</html>
