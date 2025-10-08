  
<html>
    <head>
         <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
         <meta name="layout" content="main" />
         <title>Create User</title>         
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a href="${createLinkTo(dir:'')}">Home</a></span>
        </div>
        <div class="body">
           <h1>Create User</h1>
           <g:if test="${flash.message}">
                 <div class="message">${flash.message}</div>
           </g:if>
           <g:hasErrors bean="${user}">
                <div class="errors">
                    <g:renderErrors bean="${user}" as="list" />
                </div>
           </g:hasErrors>
           <g:form action="save" method="post" >
               <div class="dialog">
                <table>
                    <tbody>

                                  <tr class='prop'><td valign='top' class='name'><label for='email'>Email:</label></td><td valign='top' class='value ${hasErrors(bean:user,field:'email','errors')}'><input type="text" name='email' size='64' maxlength='255' value='${user?.email?.encodeAsHTML()}'></td></tr>

                                  <tr class='prop'><td valign='top' class='name'><label for='password'>Password:</label></td><td valign='top' class='value ${hasErrors(bean:user,field:'password','errors')}'><input type="password" name='password' size='16' maxlength='255'></td></tr>

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
