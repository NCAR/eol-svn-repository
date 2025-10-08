<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%
   response.addHeader("Pragma","no-cache");
   response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
   response.addHeader("Cache-Control","pre-check=0, post-check=0");
   response.setDateHeader("Expires",0);
%>

<c:choose>
   <c:when test="${param.project == 'ACE-ASIA'}">
      <c:import url="file:/net/web/data/html/master_lists/static/ace-asia/index.html" />
   </c:when>
   <c:when test="${param.project == 'ADELE_SPRITE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/adele_sprite/index.html" />
   </c:when>
   <c:when test="${param.project == 'AMMA'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/amma/index.html" />
   </c:when>
   <c:when test="${param.project == 'BAMEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/bamex/index.html" />
   </c:when>
   <c:when test="${param.project == 'CEOP/EOP-1'}">
      <c:import url="file:/net/web/data/html/master_lists/static/ceop/eop1/index.html" />
   </c:when>
   <c:when test="${param.project == 'CEOP/EOP-3/4'}">
      <c:import url="file:/net/web/data/html/master_lists/static/ceop/eop3and4/index.html" />
   </c:when>
   <c:when test="${param.project == 'CEOP/HYDRO'}">
      <c:import url="file:/net/web/data/html/master_lists/static/ceop/hydro/index.html" />
   </c:when>
   <c:when test="${param.project == 'CuPIDO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/cupido/index.html" />
   </c:when>
   <c:when test="${param.project == 'DC-3_TEST'}">
      <c:import url="file:/net/web/data/html/master_lists/static/dc-3_test/index.html" />
   </c:when>
   <c:when test="${param.project == 'DYCOMS'}">
      <c:import url="file:/net/web/data/html/master_lists/static/dycoms/index.html" />
   </c:when>
   <c:when test="${param.project == 'EPIC'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/epic/index.html" />
   </c:when>
   <c:when test="${param.project == 'GAPP'}">
      <c:import url="file:/net/web/data/html/master_lists/static/gapp/index.html" />
   </c:when>
   <c:when test="${param.project == 'GCIP'}">
      <c:import url="file:/net/web/data/html/master_lists/static/gcip/index.html" />
   </c:when>
   <c:when test="${param.project == 'HIPPO-1'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/hippo-1/index.html" />
   </c:when>
   <c:when test="${param.project == 'HIPPO-2'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/hippo-2/index.html" />
   </c:when>
   <c:when test="${param.project == 'HIPPO-3'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/hippo-3/index.html" />
   </c:when>
   <c:when test="${param.project == 'HIPPO-4'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/hippo-4/index.html" />
   </c:when>
   <c:when test="${param.project == 'HIPPO-5'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/hippo-5/index.html" />
   </c:when>
   <c:when test="${param.project == 'ICE-L'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ice-l/index.html" />
   </c:when>
   <c:when test="${param.project == 'ICE-T'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ice-t/index.html" />
   </c:when>
   <c:when test="${param.project == 'IDEAS-1'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ideas-1/index.html" />
   </c:when>
   <c:when test="${param.project == 'IDEAS-2'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ideas-2/index.html" />
   </c:when>
   <c:when test="${param.project == 'IDEAS-3'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ideas-3/index.html" />
   </c:when>
   <c:when test="${param.project == 'IHOP_2002'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ihop/index.html" />
   </c:when>
   <c:when test="${param.project == 'INDOEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/indoex/index.html" />
   </c:when>
   <c:when test="${param.project == 'ITEX'}">
      <c:import url="file:/net/www/docs/projects/itex/itex_data_table.html" />
   </c:when>
   <c:when test="${param.project == 'ITOP'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/itop/index.html" />
   </c:when>
   <c:when test="${param.project == 'LPB'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/lpb/index.html" />
   </c:when>
   <c:when test="${param.project == 'MILAGRO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/milagro/index.html" />
   </c:when>
   <c:when test="${param.project == 'NAME'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/name/index.html" />
   </c:when>
   <c:when test="${param.project == 'OHHI'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ohhi/index.html" />
   </c:when>
   <c:when test="${param.project == 'PACDEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/pacdex/index.html" />
   </c:when>
   <c:when test="${param.project == 'PACS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/pacs/index.html" />
   </c:when>
   <c:when test="${param.project == 'PASE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/pase/index.html" />
   </c:when>
   <c:when test="${param.project == 'PLATIN'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/lpb/index.html" />
   </c:when>
   <c:when test="${param.project == 'PLOWS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/plows/index.html" />
   </c:when>
   <c:when test="${param.project == 'POST'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/post/index.html" />
   </c:when>
   <c:when test="${param.project == 'PREDICT'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/predict/index.html" />
   </c:when>
   <c:when test="${param.project == 'RAINEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/rainex/index.html" />
   </c:when>
   <c:when test="${param.project == 'RICO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/rico/index.html" />
   </c:when>
   <c:when test="${param.project == 'SALLJEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/salljex/index.html" />
   </c:when>
   <c:when test="${param.project == 'SMEX'}">
      <c:import url="file:/net/web/data/html/master_lists/static/smex/index.html" />
   </c:when>
   <c:when test="${param.project == 'START08'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/start08/index.html" />
   </c:when>
   <c:when test="${param.project == 'T-28'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/t28/index.html" />
   </c:when>
   <c:when test="${param.project == 'T-PARC'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/t-parc/index.html" />
   </c:when>
   <c:when test="${param.project == 'T-REX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/trex/index.html" />
   </c:when>
   <c:when test="${param.project == 'TIMREX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/timrex/index.html" />
   </c:when>
   <c:when test="${param.project == 'VOCALS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vocals/index.html" />
   </c:when>
   <c:when test="${param.project == 'VORTEX2'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vortex2/index.html" />
   </c:when>
   <c:when test="${param.project == 'WAMME'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/wamme/index.html" />
   </c:when>
   <c:otherwise>
<html>
<head>
   <title>Master List</title>
</head>

<body>
<h1>Master List</h1>
<c:if test="${!empty param.project && param.project != ''}">
   <p>The Master List for ${param.project} could not be found.</p>
</c:if>

<ul>
   <li><a href="/master_list/?project=ACE-ASIA">ACE-ASIA</a></li>
   <li><a href="/master_list/?project=ADELE_SPRITE">ADELE_SPRITE</a></li>
   <li><a href="/master_list/?project=AMMA">AMMA</a></li>
   <li><a href="/master_list/?project=BAMEX">BAMEX</a></li>
   <li><a href="/master_list/?project=CEOP/EOP-1">CEOP/EOP-1</a></li>
   <li><a href="/master_list/?project=CEOP/EOP-3/4">CEOP/EOP-3/4</a></li>
   <li><a href="/master_list/?project=CEOP/HYDRO">CEOP/HYDRO</a></li>
   <li><a href="/master_list/?project=CuPIDO">CuPIDO</a></li>
   <li><a href="/master_list/?project=DC-3_TEST">DC-3_TEST</a></li>
   <li><a href="/master_list/?project=DYCOMS">DYCOMS</a></li>
   <li><a href="/master_list/?project=EPIC">EPIC</a></li>
   <li><a href="/master_list/?project=GAPP">GAPP</a></li>
   <li><a href="/master_list/?project=GCIP">GCIP</a></li>
   <li><a href="/master_list/?project=HIPPO-1">HIPPO-1</a></li>
   <li><a href="/master_list/?project=HIPPO-2">HIPPO-2</a></li>
   <li><a href="/master_list/?project=HIPPO-3">HIPPO-3</a></li>
   <li><a href="/master_list/?project=HIPPO-4">HIPPO-4</a></li>
   <li><a href="/master_list/?project=HIPPO-5">HIPPO-5</a></li>
   <li><a href="/master_list/?project=ICE-L">ICE-L</a></li>
   <li><a href="/master_list/?project=ICE-T">ICE-T</a></li>
   <li><a href="/master_list/?project=IDEAS-1">IDEAS-1</a></li>
   <li><a href="/master_list/?project=IDEAS-2">IDEAS-2</a></li>
   <li><a href="/master_list/?project=IDEAS-3">IDEAS-3</a></li>
   <li><a href="/master_list/?project=IHOP_2002">IHOP_2002</a></li>
   <li><a href="/master_list/?project=INDOEX">INDOEX</a></li>
   <li><a href="/master_list/?project=ITEX">ITEX</a></li>
   <li><a href="/master_list/?project=ITOP">ITOP</a></li>
   <li><a href="/master_list/?project=LPB">LPB</a></li>
   <li><a href="/master_list/?project=MILAGRO">MILAGRO</a></li>
   <li><a href="/master_list/?project=NAME">NAME</a></li>
   <li><a href="/master_list/?project=OHHI">OHHI</a></li>
   <li><a href="/master_list/?project=PACDEX">PACDEX</a></li>
   <li><a href="/master_list/?project=PACS">PACS</a></li>
   <li><a href="/master_list/?project=PASE">PASE</a></li>
   <li><a href="/master_list/?project=PLATIN">PLATIN</a></li>
   <li><a href="/master_list/?project=PLOWS">PLOWS</a></li>
   <li><a href="/master_list/?project=POST">POST</a></li>
   <li><a href="/master_list/?project=PREDICT">PREDICT</a></li>
   <li><a href="/master_list/?project=RAINEX">RAINEX</a></li>
   <li><a href="/master_list/?project=RICO">RICO</a></li>
   <li><a href="/master_list/?project=SALLJEX">SALLJEX</a></li>
   <li><a href="/master_list/?project=SMEX">SMEX</a></li>
   <li><a href="/master_list/?project=START08">START08</a></li>
   <li><a href="/master_list/?project=T-28">T-28</a></li>
   <li><a href="/master_list/?project=T-PARC">T-PARC</a></li>
   <li><a href="/master_list/?project=T-REX">T-REX</a></li>
   <li><a href="/master_list/?project=TIMREX">TIMREX</a></li>
   <li><a href="/master_list/?project=VOCALS">VOCALS</a></li>
   <li><a href="/master_list/?project=VORTEX2">VORTEX2</a></li>
   <li><a href="/master_list/?project=WAMME">WAMME</a></li>
</ul>

</body>
</html>
   </c:otherwise>
</c:choose>
