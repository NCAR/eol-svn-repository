  
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>GcmdInstrument List</title>
    </head>
    <body>
        <div class="nav" align="center">
            <span class="menuButton"><g:link class="list" action="list">GcmdInstrument List</g:link></span>
            <g:render template="return_nav" />
        </div>
        <div class="body">
            <h1>GcmdInstrument List</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="list">
                <table>
                    <thead>
                        <tr>
                        
                   	        <g:sortableColumn property="id" title="Id" />
                        
                   	        <g:sortableColumn property="keyword" title="Keyword" />
                        
                        </tr>
                    </thead>
                    <tbody>
                    <g:each in="${gcmdInstrumentList}" status="i" var="gcmdInstrument">
                        <tr class="${(i % 2) == 0 ? 'odd' : 'even'}">
                        
                            <td><g:link action="show" id="${gcmdInstrument.id}">${gcmdInstrument.id?.encodeAsHTML()}</g:link></td>
                        
                            <td>${gcmdInstrument.keyword?.encodeAsHTML()}</td>
                        
                        </tr>
                    </g:each>
                    </tbody>
                </table>
            </div>
            <div class="paginateButtons">
                <g:paginate total="${GcmdInstrument.count()}" />
            </div>
        </div>
    </body>
</html>
