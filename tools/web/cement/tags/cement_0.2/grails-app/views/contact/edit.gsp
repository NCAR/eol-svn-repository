  
<html>
    <head>
         <title>Edit Contact</title>
         <meta name="layout" content="formpage" />
    </head>

    <body>
        <div class="body">
           <h1>Edit Contact</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${contact}">
                <div class="errors">
                    <g:renderErrors bean="${contact}" as="list" />
                </div>
           </g:hasErrors>
           <div class="prop">
	      <span class="name">Id:</span>
	      <span class="value">${contact?.id}</span>
           </div>           
           <g:form controller="contact" method="post" >
               <input type="hidden" name="id" value="${contact?.id}" />
               <div class="dialog">
                <table>
                    <tbody>

				<tr class='prop'><td valign='top' class='name'><label for='personName'>Person Name:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'personName','errors')}'><input type="text" name='personName' value="${contact?.personName?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='organizationName'>Organization Name:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'organizationName','errors')}'><input type="text" name='organizationName' value="${contact?.organizationName?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='shortName'>Short Name:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'shortName','errors')}'><input type="text" name='shortName' value="${contact?.shortName?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='address'>Address:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'address','errors')}'><input type="text" name='address' value="${contact?.address?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='city'>City:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'city','errors')}'><input type="text" name='city' value="${contact?.city?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='state'>State:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'state','errors')}'><input type="text" name='state' value="${contact?.state?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='postalCode'>Postal Code:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'postalCode','errors')}'><input type="text" name='postalCode' value="${contact?.postalCode?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='email'>Email:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'email','errors')}'><input type="text" name='email' value="${contact?.email?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='homePage'>Home Page:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'homePage','errors')}'><input type="text" name='homePage' value="${contact?.homePage?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='faxNumber'>Fax Number:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'faxNumber','errors')}'><input type="text" name='faxNumber' value="${contact?.faxNumber?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='gcmdName'>Gcmd Name:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'gcmdName','errors')}'><input type="text" name='gcmdName' value="${contact?.gcmdName?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='metadataDatasets'>Metadata Datasets:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'metadataDatasets','errors')}'><ul>
    <g:each var='m' in='${contact?.metadataDatasets?}'>
        <li><g:link controller='dataset' action='show' id='${m.id}'>${m}</g:link></li>
    </g:each>
</ul>
<g:link controller='dataset' params='["contact.id":contact?.id]' action='create'>Add Dataset</g:link>
</td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='department'>Department:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'department','errors')}'><input type="text" name='department' value="${contact?.department?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='country'>Country:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'country','errors')}'><input type="text" name='country' value="${contact?.country?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='datacenterDatasets'>Datacenter Datasets:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'datacenterDatasets','errors')}'><ul>
    <g:each var='d' in='${contact?.datacenterDatasets?}'>
        <li><g:link controller='dataset' action='show' id='${d.id}'>${d}</g:link></li>
    </g:each>
</ul>
<g:link controller='dataset' params='["contact.id":contact?.id]' action='create'>Add Dataset</g:link>
</td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='phoneNumber'>Phone Number:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'phoneNumber','errors')}'><input type="text" name='phoneNumber' value="${contact?.phoneNumber?.encodeAsHTML()}"/></td></tr>
                       
				<tr class='prop'><td valign='top' class='name'><label for='piDatasets'>Pi Datasets:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'piDatasets','errors')}'><ul>
    <g:each var='p' in='${contact?.piDatasets?}'>
        <li><g:link controller='dataset' action='show' id='${p.id}'>${p}</g:link></li>
    </g:each>
</ul>
<g:link controller='dataset' params='["contact.id":contact?.id]' action='create'>Add Dataset</g:link>
</td></tr>
                       
                    </tbody>
                </table>
               </div>

               <div class="buttons">
                     <span class="button"><g:actionSubmit value="Update" /></span>
                 	 <span class="button"><g:actionSubmit value="Delete" onclick="return warnBeforeContactDelete();" /></span>
               </div>
            </g:form>
        </div>
      </table>
      <p>&nbsp;</p>
      <p>&nbsp;</p>
      </td>

    </body>
</html>
