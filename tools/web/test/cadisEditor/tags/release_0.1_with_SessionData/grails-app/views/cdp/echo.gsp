<html>
<body>
<h1>attribute echo page</h1>

<g:set var="ctx_name" value="${application?.getContextPath()}" />
<g:set var="attr_name" value="cadis.xml.name_echokey" />
<g:set var="attr_data" value="cadis.xml.data_echokey" />

<g:set var="nnn" value="${application?.getContext(ctx_name)?.getAttribute(attr_name)}" />
<g:set var="ddd" value="${application?.getContext(ctx_name)?.getAttribute(attr_data)}" />

<p>context = ${ctx_name}</p>
<p>${attr_name} = ${nnn}</p>
<p>${attr_data} = ${ddd.encodeAsHTML()}</p>

</body>
</html>
