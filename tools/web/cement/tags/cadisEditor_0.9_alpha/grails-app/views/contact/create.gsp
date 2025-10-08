  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="contact_create" />
         <title>Create Contact</title>         
    </head>
    <body>
        <div class="body">
           <h1>Create Contact</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${contact}">
                <div class="errors">
                    <g:renderErrors bean="${contact}" as="list" />
                </div>
           </g:hasErrors>
           <g:form action="save" method="post" >
               <div class="dialog">
                <table>
                    <tbody>

                                  <tr class='req'><td valign='top' class='name'><label for='personName'>Person Name:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'personName','errors')}'><input type="text" name='personName' value="${contact?.personName?.encodeAsHTML()}"/></td></tr>
                        
                                  <tr class='req'><td valign='top' class='name'><label for='shortName'>Short Name:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'shortName','errors')}'><input type="text" name='shortName' value="${contact?.shortName?.encodeAsHTML()}"/></td></tr>
                        
                                  <tr class='req'><td valign='top' class='name'><label for='email'>Email:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'email','errors')}'><input type="text" name='email' value="${contact?.email?.encodeAsHTML()}"/></td></tr>
                     
                                  <tr class='req'><td valign='top' class='name'><label for='organizationName'>Organization Name:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'organizationName','errors')}'><input type="text" name='organizationName' value="${contact?.organizationName?.encodeAsHTML()}"/></td></tr>
 
                                  <tr class='prop'><td valign='top' class='name'><label for='department'>Department:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'department','errors')}'><input type="text" name='department' value="${contact?.department?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='position'>Position:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'position','errors')}'><input type="text" name='position' value="${contact?.position?.encodeAsHTML()}"/></td></tr>
                      
                                  <tr class='prop'><td valign='top' class='name'><label for='address'>Address:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'address','errors')}'><input type="text" name='address' value="${contact?.address?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='city'>City:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'city','errors')}'><input type="text" name='city' value="${contact?.city?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='state'>State or Province:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'state','errors')}'><input type="text" name='state' value="${contact?.state?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='country'>Country:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'country','errors')}'><input type="text" name='country' value="${contact?.country?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='postalCode'>Postal Code:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'postalCode','errors')}'><input type="text" name='postalCode' value="${contact?.postalCode?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='phoneNumber'>Phone Number:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'phoneNumber','errors')}'><input type="text" name='phoneNumber' value="${contact?.phoneNumber?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='faxNumber'>Fax Number:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'faxNumber','errors')}'><input type="text" name='faxNumber' value="${contact?.faxNumber?.encodeAsHTML()}"/></td></tr>
                       
                                  <tr class='prop'><td valign='top' class='name'><label for='homePage'>Home Page:</label></td><td valign='top' class='value ${hasErrors(bean:contact,field:'homePage','errors')}'><input type="text" name='homePage' value="${contact?.homePage?.encodeAsHTML()}"/></td></tr>
                       
                    </tbody>
               </table>
               </div>
               <div class="buttons">
                     <span class="formButton">
                        <input type="submit" value="Create"></input>
                    	<span class="button">&nbsp;<g:link action="list">Cancel</g:link></span><br>
                     </span>
               </div>
            </g:form>
        </div>
    </body>
</html>
