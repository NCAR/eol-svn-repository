<html> 
    <head> 
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/> 
        <meta name="layout" content="public" /> 
        <title>show stuff</title> 
    </head> 
    <body> 
        <div class="body"> 
            <g:if test="${flash.message}"> 
                <div class="message">${flash.message}</div> 
            </g:if> 
<p>
stuff I found:<br />
<pre>
${bigstring.encodeAsHTML()}
</pre>
</p>
        </div> 
    </body> 
</html> 
