

<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <meta name="layout" content="main" />
        <title>XlinkType List</title>
    </head>
    <body>
        <div class="nav">
            <span class="menuButton"><a class="home" href="${createLinkTo(dir:'')}">Home</a></span>
            <span class="menuButton"><g:link class="create" action="create">New XlinkType</g:link></span>
        </div>
        <div class="body">
            <h1>XlinkType List</h1>
            <g:if test="${flash.message}">
            <div class="message">${flash.message}</div>
            </g:if>
            <div class="list">
                <table>
                    <thead>
                        <tr>
                        
                   	        <g:sortableColumn property="id" title="Id" />
                        
                   	        <g:sortableColumn property="purpose" title="Purpose" />
                        
                        </tr>
                    </thead>
                    <tbody>
                    <g:each in="${xlinkTypeList}" status="i" var="xlinkType">
                        <tr class="${(i % 2) == 0 ? 'odd' : 'even'}">
                        
                            <td><g:link action="show" id="${xlinkType.id}">${xlinkType.id?.encodeAsHTML()}</g:link></td>
                        
                            <td>${xlinkType.purpose?.encodeAsHTML()}</td>
                        
                        </tr>
                    </g:each>
                    </tbody>
                </table>
            </div>
            <div class="paginateButtons">
                <g:paginate total="${XlinkType.count()}" />
            </div>
        </div>
    </body>
</html>
