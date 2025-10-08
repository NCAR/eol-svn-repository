<% response.contentType = "text/xml" %> 
<% response.characterEncoding = "UTF-8" %>
<metadata
 xmlns="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0 http://www.unidata.ucar.edu/schemas/thredds/InvCatalog.1.0.xsd"
 metadataType="THREDDS" inherited="true">

<documentation type="title">${dataset.title?.encodeAsHTML()}</documentation>
<documentation type="summary">${dataset.summary?.encodeAsHTML()}</documentation>

<geospatialCoverage>
 <northsouth>
  <start>${dataset.minimumLatitude}</start>
  <size>${dataset.maximumLatitude - dataset.minimumLatitude}</size>
  <units>Degrees</units>
 </northsouth>
 <eastwest>
  <start>${dataset.minimumLongitude}</start>
  <size>${dataset.maximumLongitude - dataset.minimumLongitude}</size>
  <units>Degrees</units>
 </eastwest>
</geospatialCoverage>

<timeCoverage>
 <start><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${dataset.beginDate}" /></start>
 <end><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${dataset.endDate}" /></end>
</timeCoverage>

<publisher>
 <name>NCAR/EOL</name>
 <contact url="http://data.eol.ucar.edu/" email="codiac@ucar.edu"/>
</publisher>

<contributor role="pointOfContact">
${dataset.piContact?.toCdpString()}
</contributor>

<contributor role="pointOfContact">
${dataset.metadataContact?.toCdpString()}
</contributor>

<contributor role="custodian">
${dataset.datacenterContact?.toCdpString()}
</contributor>

<keyword vocabulary="DIF">${dataset.scienceKeyword}</keyword>
<keyword vocabulary="DIF">${dataset.locationKeyword}</keyword>
<project vocabulary="DIF">${dataset.projectKeyword}</project>

<documentation type="funding">NSF Award Number ${dataset.nsfAwardNumber}</documentation>

<property name="progress" value="${dataset.progress}" />
<keyword>${dataset.discipline}</keyword>

<cdp:dataType type="${dataset.spatialType}" />

</metadata>
