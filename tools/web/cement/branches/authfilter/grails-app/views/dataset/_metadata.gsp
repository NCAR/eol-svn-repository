<metadata
 xmlns="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0"
 xmlns:xlink="http://www.w3.org/1999/xlink"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0 http://www.unidata.ucar.edu/schemas/thredds/InvCatalog.1.0.xsd"
 metadataType="THREDDS" inherited="true">

<documentation type="title">${it.title?.encodeAsHTML()}</documentation>
<documentation type="summary">${it.summary?.encodeAsHTML()}</documentation>

<geospatialCoverage>
 <northsouth>
  <start>${it.minimumLatitude}</start>
  <size>${it.maximumLatitude - it.minimumLatitude}</size>
  <units>Degrees</units>
 </northsouth>
 <eastwest>
  <start>${it.minimumLongitude}</start>
  <size>${it.maximumLongitude - it.minimumLongitude}</size>
  <units>Degrees</units>
 </eastwest>
</geospatialCoverage>

<timeCoverage>
 <start><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.beginDate}" /></start>
 <end><g:formatDate format="yyyy-MM-DD'T'HH:mm:ss'Z'" date="${it.endDate}" /></end>
</timeCoverage>

<publisher>
 <name>NCAR/EOL</name>
 <contact url="http://data.eol.ucar.edu/" email="codiac@ucar.edu"/>
</publisher>

<contributor role="pointOfContact">
${it.piContact?.toCdpString()}
</contributor>

<contributor role="pointOfContact">
${it.metadataContact?.toCdpString()}
</contributor>

<contributor role="custodian">
${it.datacenterContact?.toCdpString()}
</contributor>

<keyword vocabulary="DIF">${it.scienceKeyword}</keyword>
<keyword vocabulary="DIF">${it.locationKeyword}</keyword>
<project vocabulary="DIF">${it.projectKeyword}</project>

<documentation type="funding">NSF Award Number ${it.nsfAwardNumber}</documentation>

<property name="progress" value="${it.progress}" />
<keyword>${it.discipline}</keyword>

<cdp:dataType type="${it.spatialType}" />

</metadata>
