<html>
    <head>
        <title>Hello, CDP!</title>
    </head>
    <body>
        <h1>Hello, CDP!</h1>

<g:set var='foo' value='${new ncar.scd.security.SessionData(1, "u", "e", "l", "p", "s", null)}' />
<p>foo user = ${foo.username}</p>
<p>${foo.class.classLoader}</p>

<g:set var="attr" value="${null}" />
<g:set var="sd" value="${null}" />
<g:set var="attrlist" value="[]" />

<!-- XXX change or add to ctxnames whatever CDP printed out for getServletContextPath() -->
<g:set var='ctxnames' value='["/cdp", "/","/portal"]' />

<!-- XXX change or add to cooknames whatever cookie(s) that CDP sets -->
<g:set var='cooknames' value='["JSESSIONIDSSO","JSESSIONID"]' />

<p>params.sid = ${params.sid}</p>
<g:if test="${params.sid}">
<g:set var="attrlist" value="${attrlist + params.sid}" />
</g:if>

<p>params.sid2 = ${params.sid2}</p>
<g:if test="${params.sid2}">
<g:set var="attrlist" value="${attrlist + params.sid2}" />
</g:if>

<p>Cookies:<table border="1"><tr><th>name</th><th>value</th><th>path</th><th>domain</th></tr>
<g:each var="reqcook" in="${request.cookies}">
<tr><td>${reqcook.name}</td><td>${reqcook.value}</td><td>${reqcook.path}</td><td>${reqcook.domain}</td></tr>
 <g:each var="wantcook" in="${cooknames}">
 <g:if test="${reqcook.name == wantcook}">
 <g:set var="attrlist" value="${attrlist + reqcook.value}" />
 </g:if>
 </g:each>
</g:each>
</table></p>

<p>Attributes: ${attrlist}</p>

<g:each var="ctxname" in="${ctxnames}">
<p><hr>Looking in context ${ctxname}</p>

<g:each var="atid" in="${attrlist}">

<g:set var='attr' value='${"session.data_"+atid}' />
<p>Looking for attribute ${attr}</p>
<g:set var="sd" value="${application?.getContext(ctxname)?.getAttribute(attr)}" />
<g:if test="${sd==null}">
not found
</g:if>
<g:else>
<p>yes found :::${sd}:::</p>
<p>sd.class = ${sd.class}</p>
<p>sd.class.classLoader = ${sd.class.classLoader}</p>
<g:if test='${sd instanceof ncar.scd.security.SessionData}'>
<p><g:link action="entry" controller="cdp" params="[sid:atid]">go to entry</g:link></p>
<p>sd.id = ${sd.id}</p>
<p>sd.sessionId = ${sd.sessionId}</p>
<p>sd.username = ${sd.username}</p>
<p>sd.email = ${sd.email}</p>
<p>sd.loginType = ${sd.loginType}</p>
<p>sd.portalUrl = ${sd.portalUrl}</p>
<p>sd.permissions.toString() = ${sd.permissions.toString()}</p>
<p>sd.permissions.getClass() = ${sd.permissions.getClass()}</p>
<p>sd.permissions =<ul>
<g:each in="${sd.permissions}">
<li>${it.key} : ${it.value}</li>
</g:each>
</ul></p>
</g:if>
</g:else>

</g:each>
</g:each>


    </body>
</html>
