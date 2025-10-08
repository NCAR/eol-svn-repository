<html> 
    <head> 
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/> 
        <meta name="layout" content="public" /> 
        <title>Log in</title> 
    </head> 
    <body> 
        <div class="body"> 
            <h1>Please log in</h1> 
            <g:if test="${flash.message}"> 
                <div class="message">${flash.message}</div> 
            </g:if> 
            <g:form controller="auth" method="post" > 
                <div class="dialog"> 
                <table> 

                    <tr class='prop'> 
                        <td valign='top' class='name'> 
                            <label for='username'>Username:</label> 
                        </td> 
                        <td valign='top' class='value '> 
                            <input type="text" maxlength='255' name='username'
                                value='${contact?.shortName}'></input>
                    </tr> 

                    <tr class='prop'> 
                        <td valign='top' class='name'> 
                            <label for='fullName'>Full Name:</label> 
                        </td> 
                        <td valign='top' class='value '> 
                            <input type="text" maxlength='255' name='fullName'
			        value='${contact?.personName}'></input>
				*only required if your username is not in cement contact database
                    </tr> 

                    <tr class='prop'> 
                        <td valign='top' class='name'> 
                            <label for='email'>Email:</label> 
                        </td> 
                        <td valign='top' class='value '> 
                            <input type="text" maxlength='255' name='email'
                                value='${contact?.email}'></input>
				*only required if your username is not in cement contact database
                    </tr> 

                    <tr class='prop'> 
                        <td valign='top' class='name'> 
                            <label for='userInstitution'>Organization:</label> 
                        </td> 
                        <td valign='top' class='value '> 
                            <input type="text" maxlength='255' name='userInstitution'
                                value='${contact?.organizationName}'></input>
				*only required if your username is not in cement contact database
                    </tr> 

                    <tr class='prop'> 
                        <td valign='top' class='name'> 
                            <label for='datasetId'>Dataset ID:</label> 
                        </td> 
                        <td valign='top' class='value '> 
                            <input type="text" maxlength='255' name='datasetId'
			        value='${datasetId}'></input>
				*optional
                    </tr> 

                    <tr class='prop'> 
                        <td valign='top' class='name'> 
                            <label for='cadisPrivilegesStr'>Cadis Privileges:</label> 
                        </td> 
                        <td valign='top' class='value '> 
                            <input type="text" maxlength='255' name='cadisPrivilegesStr'
			        value='${cadisPrivilegesStr}'></input>
				*optional
                    </tr> 

                </table> 
                </div> 
                <div class="buttons"> 
                <span class="button"><g:actionSubmit action="Login" value="Log in" /></span> 
                </div> 
            </g:form> 
        </div> 
    </body> 
</html> 
