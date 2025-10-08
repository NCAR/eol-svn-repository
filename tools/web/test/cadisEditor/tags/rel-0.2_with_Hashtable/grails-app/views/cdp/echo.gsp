<html>
<head>
<title>cadisEditor echo page</title>
</head>
<body>
<h1>cadisEditor echo page</h1>

<g:set var="ctx_name" value="${application?.getContextPath()}" />
<g:set var="attr" value="session.cadisEditor" />

<g:set var="ht" value="${application?.getContext(ctx_name)?.getAttribute(attr)}" />
<g:set var="hh" value="${ht['echokey']}" />

<p>context = ${ctx_name}</p>
<p>ht = ${ht.encodeAsHTML()}</p>
<p>hh = ${hh.encodeAsHTML()}</p>
<p>username = ${hh['username'].encodeAsHTML()}</p>
<p>class = ${hh['username'].getClass()}</p>
<p>datasetId = ${hh['datasetId'].encodeAsHTML()}</p>
<p>class = ${hh['datasetId'].getClass()}</p>
<p>metadata.xml = ${hh['metadata.xml'].encodeAsHTML()}</p>
<p>class = ${hh['metadata.xml'].getClass()}</p>

</body>
</html>
