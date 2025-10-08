<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<%
   response.addHeader("Pragma","no-cache");
   response.setHeader("Cache-Control","no-cache, no-store, must-revalidate");
   response.addHeader("Cache-Control","pre-check=0, post-check=0");
   response.setDateHeader("Expires",0);
%>

<c:choose>
   <c:when test="${param.project == 'ACE-ENA'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ace-ena/index.html" />
   </c:when>
   <c:when test="${param.project == 'ACE-ASIA'}">
      <c:import url="file:/net/web/data/html/master_lists/static/ace-asia/index.html" />
   </c:when>
   <c:when test="${param.project == 'ADELE_SPRITE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/adele_sprite/index.html" />
   </c:when>
   <c:when test="${param.project == 'AMMA'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/amma/index.html" />
   </c:when>
   <c:when test="${param.project == 'ARISTO2015'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/aristo2015/index.html" />
   </c:when>
   <c:when test="${param.project == 'ARISTO2016'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/aristo2016/index.html" />
   </c:when>
   <c:when test="${param.project == 'ARISTO2017'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/aristo2017/index.html" />
   </c:when>
   <c:when test="${param.project == 'ASPIRE-TEST'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/aspire-test/index.html" />
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
   <c:when test="${param.project == 'CASES-97'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/cases-97/index.html" />
   </c:when>
   <c:when test="${param.project == 'CASES-99'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/cases-99/index.html" />
   </c:when>
   <c:when test="${param.project == 'CFACT'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/cfact/index.html" />
   </c:when>
   <c:when test="${param.project == 'CONTRAST'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/contrast/index.html" />
   </c:when>
   <c:when test="${param.project == 'CSET'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/cset/index.html" />
   </c:when>
   <c:when test="${param.project == 'CHEESEHEAD'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/cheesehead/index.html" />
   </c:when>
   <c:when test="${param.project == 'DC3'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/dc3/index.html" />
   </c:when>
   <c:when test="${param.project == 'DEEPWAVE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/deepwave/index.html" />
   </c:when>
   <c:when test="${param.project == 'DYCOMS'}">
      <c:import url="file:/net/web/data/html/master_lists/static/dycoms/index.html" />
   </c:when>
   <c:when test="${param.project == 'DYNAMO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/dynamo/index.html" />
   </c:when>
   <c:when test="${param.project == 'ECLIPSE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/eclipse/index.html" />
   </c:when>
   <c:when test="${param.project == 'ECLIPSE2019'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/eclipse2019/index.html" />
   </c:when>
   <c:when test="${param.project == 'EPIC'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/epic/index.html" />
   </c:when>
   <c:when test="${param.project == 'FRAPPE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/frappe/index.html" />
   </c:when>
   <c:when test="${param.project == 'FRONT'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/front/index.html" />
   </c:when>
   <c:when test="${param.project == 'FRONT-DE2'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/front-de2/index.html" />
   </c:when>
   <c:when test="${param.project == 'FRONT-ROSE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/front-rose/index.html" />
   </c:when>                                                           
   <c:when test="${param.project == 'FRONT-STEP'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/front-step/index.html" />
   </c:when>
   <c:when test="${param.project == 'GAPP'}">
      <c:import url="file:/net/web/data/html/master_lists/static/gapp/index.html" />
   </c:when>
   <c:when test="${param.project == 'GCIP'}">
      <c:import url="file:/net/web/data/html/master_lists/static/gcip/index.html" />
   </c:when>
   <c:when test="${param.project == 'GRAINEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/grainex/index.html" />
   </c:when>
   <c:when test="${param.project == 'HAIC-HIWC'}">
   <c:import url="file:/net/web/data/html/master_lists/generated/haic-hiwc/index.html" />
   </c:when>
   <c:when test="${param.project == 'HAIC-HIWC_2015'}">
   <c:import url="file:/net/web/data/html/master_lists/generated/haic-hiwc_2015/index.html" />
    </c:when>
   <c:when test="${param.project == 'HCR-Test'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/hcrtest/index.html" />
   </c:when>
   <c:when test="${param.project == 'HIGHWAY'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/highway/index.html" />
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
   <c:when test="${param.project == 'HIWC-FL'}">
   <c:import url="file:/net/web/data/html/master_lists/generated/hiwc-fl/index.html" />
   </c:when>
   <c:when test="${param.project == 'HIWC-RADAR-2018'}">
   <c:import url="file:/net/web/data/html/master_lists/generated/hiwc-radar-2018/index.html" />
   </c:when>
   <c:when test="${param.project == 'ICE-L'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ice-l/index.html" />
   </c:when>
   <c:when test="${param.project == 'ICE-T'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ice-t/index.html" />
   </c:when>
   <c:when test="${param.project == 'ICEBRIDGE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/icebridge/index.html" />
   </c:when>
   <c:when test="${param.project == 'ICEBRIDGE-2015'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/icebridge-2015/index.html" />
   </c:when>
   <c:when test="${param.project == 'ICICLE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/icicle/index.html" />
   </c:when>
   <c:when test="${param.project == 'IDEAL'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ideal/index.html" />
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
   <c:when test="${param.project == 'IDEAS-4_C130'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ideas-4_c130/index.html" />
   </c:when>
   <c:when test="${param.project == 'IDEAS-4_GV'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ideas-4_gv/index.html" />
   </c:when>
   <c:when test="${param.project == 'IFRACS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ifracs/index.html" />
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
   <c:when test="${param.project == 'LATTE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/latte/index.html" />
   </c:when>
   <c:when test="${param.project == 'LPB'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/lpb/index.html" />
   </c:when>
   <c:when test="${param.project == 'MASCRAD'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/mascrad/index.html" />
   </c:when>
   <c:when test="${param.project == 'MATERHORN-X'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/materhorn-x/index.html" />
   </c:when>
   <c:when test="${param.project == 'Meso18-19'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/meso18-19/index.html" />
   </c:when>
   <c:when test="${param.project == 'MethaneAIR'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/methaneair/index.html" />
   </c:when>
   <c:when test="${param.project == 'MILAGRO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/milagro/index.html" />
   </c:when>
   <c:when test="${param.project == 'MITTS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/mitts/index.html" />
   </c:when>
   <c:when test="${param.project == 'MPEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/mpex/index.html" />
   </c:when>
   <c:when test="${param.project == 'NAME'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/name/index.html" />
   </c:when>
   <c:when test="${param.project == 'NOREASTER'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/noreaster/index.html" />
   </c:when>
   <c:when test="${param.project == 'OHHI'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/ohhi/index.html" />
   </c:when>
   <c:when test="${param.project == 'ORCAS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/orcas/index.html" />
   </c:when>
   <c:when test="${param.project == 'OTREC'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/otrec/index.html" />
   </c:when>
   <c:when test="${param.project == 'OTREC-TEST'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/otrec-test/index.html" />
   </c:when>
   <c:when test="${param.project == 'OWLeS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/owles/index.html" />
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
   <c:when test="${param.project == 'PECAN'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/pecan/index.html" />
   </c:when>
   <c:when test="${param.project == 'PERDIGAO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/perdigao/index.html" />
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
   <c:when test="${param.project == 'RELAMPAGO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/relampago/index.html" />
   </c:when>
   <c:when test="${param.project == 'RICO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/rico/index.html" />
   </c:when>
   <c:when test="${param.project == 'SALLJEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/salljex/index.html" />
   </c:when>
   <c:when test="${param.project == 'SAANGRIA-TEST'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/saangria-test/index.html" />
   </c:when>
   <c:when test="${param.project == 'SABIRPOD'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/sabirpod/index.html" />
   </c:when>
   <c:when test="${param.project == 'SAS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/sas/index.html" />
   </c:when>
   <c:when test="${param.project == 'SAVANT'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/savant/index.html" />
   </c:when>
   <c:when test="${param.project == 'SMEX'}">
      <c:import url="file:/net/web/data/html/master_lists/static/smex/index.html" />
   </c:when>
   <c:when test="${param.project == 'SNOWIE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/snowie/index.html" />
   </c:when>
   <c:when test="${param.project == 'SOCRATES'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/socrates/index.html" />
   </c:when>
   <c:when test="${param.project == 'SOCRATES-TEST'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/socrates-test/index.html" />
   </c:when>
   <c:when tests="${param.projects == 'SPICULE'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/spicule/index.html" />
   </c:when>
   <c:when test="${param.project == 'SPRITES-II'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/sprites-ii/index.html" />
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
   <c:when test="${param.project == 'TCI'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/tci/index.html" />
   </c:when>
   <c:when test="${param.project == 'TIMREX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/timrex/index.html" />
   </c:when>
   <c:when test="${param.project == 'TORERO'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/torero/index.html" />
   </c:when>
   <c:when test="${param.project == 'TORUS_2019'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/torus_2019/index.html" />
   </c:when>
   <c:when test="${param.project == 'VERTEX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vertex/index.html" />
   </c:when>
   <c:when test="${param.project == 'VOCALS'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vocals/index.html" />
   </c:when>
   <c:when test="${param.project == 'VORTEX2'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vortex2/index.html" />
   </c:when>
   <c:when test="${param.project == 'VORTEX-SE_2016'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vortex-se_2016/index.html" />
   </c:when>
   <c:when test="${param.project == 'VORTEX-SE_2017'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vortex-se_2017/index.html" />
   </c:when>
   <c:when test="${param.project == 'VORTEX-SE_2018'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/vortex-se_2018/index.html" />
   </c:when>
   <c:when test="${param.project == 'WAMME'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/wamme/index.html" />
   </c:when>
   <c:when test="${param.project == 'WE-CAN'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/we-can/index.html" />
   </c:when>
   <c:when test="${param.project == 'WE-CAN-TEST'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/we-can-test/index.html" />
   </c:when>
   <c:when test="${param.project == 'WINTER'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/winter/index.html" />
   </c:when>
   <c:when test="${param.project == 'WINTRE-MIX'}">
      <c:import url="file:/net/web/data/html/master_lists/generated/wintre-mix/index.html" />
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
   <li><a href="/master_list/?project=ACE-ENA">ACE-ENA</a></li>
   <li><a href="/master_list/?project=ACE-ASIA">ACE-ASIA</a></li>
   <li><a href="/master_list/?project=ADELE_SPRITE">ADELE_SPRITE</a></li>
   <li><a href="/master_list/?project=AMMA">AMMA</a></li>
   <li><a href="/master_list/?project=ARISTO2015">ARISTO2015</a></li>
   <li><a href="/master_list/?project=ARISTO2016">ARISTO2016</a></li>
   <li><a href="/master_list/?project=ARISTO2017">ARISTO2017</a></li>
   <li><a href="/master_list/?project=ASPIRE-TEST">ASPIRE-TEST</a></li>
   <li><a href="/master_list/?project=BAMEX">BAMEX</a></li>
   <li><a href="/master_list/?project=CEOP/EOP-1">CEOP/EOP-1</a></li>
   <li><a href="/master_list/?project=CEOP/EOP-3/4">CEOP/EOP-3/4</a></li>
   <li><a href="/master_list/?project=CEOP/HYDRO">CEOP/HYDRO</a></li>
   <li><a href="/master_list/?project=CHEESEHEAD">CHEESEHEAD</a></li>
   <li><a href="/master_list/?project=CuPIDO">CuPIDO</a></li>
   <li><a href="/master_list/?project=CASES-97">CASES-97</a></li>
   <li><a href="/master_list/?project=CASES-99">CASES-99</a></li>
   <li><a href="/master_list/?project=CFACT">CFACT</a></li>
   <li><a href="/master_list/?project=CONTRAST">CONTRAST</a></li>
   <li><a href="/master_list/?project=CSET">CSET</a></li>
   <li><a href="/master_list/?project=DC3">DC3</a></li>
   <li><a href="/master_list/?project=DEEPWAVE">DEEPWAVE</a></li>
   <li><a href="/master_list/?project=DYCOMS">DYCOMS</a></li>
   <li><a href="/master_list/?project=DYNAMO">DYNAMO</a></li>
   <li><a href="/master_list/?project=ECLIPSE">ECLIPSE</a></li>
   <li><a href="/master_list/?project=ECLIPSE2019">ECLIPSE2019</a></li>
   <li><a href="/master_list/?project=EPIC">EPIC</a></li>
   <li><a href="/master_list/?project=FRAPPE">FRAPPE</a></li>
   <li><a href="/master_list/?project=FRONT">FRONT</a></li>
   <li><a href="/master_list/?project=FRONT-DE2">FRONT-DE2</a></li>
   <li><a href="/master_list/?project=FRONT-ROSE">FRONT-ROSE</a></li>
   <li><a href="/master_list/?project=FRONT-STEP">FRONT-STEP</a></li>
   <li><a href="/master_list/?project=GAPP">GAPP</a></li>
   <li><a href="/master_list/?project=GCIP">GCIP</a></li>
   <li><a href="/master_list/?project=GRAINEX">GRAINEX</a></li>
   <li><a href="/master_list/?project=HAIC-HIWC">HAIC-HIWC</a></li>
   <li><a href="/master_list/?project=HAIC-HIWC_2015">HAIC-HIWC_2015</a></li>
   <li><a href="/master_list/?project=HCR-Test">HCR Test</a></li>
   <li><a href="/master_list/?project=HIGHWAY">HIGHWAY</a></li>
   <li><a href="/master_list/?project=HIPPO-1">HIPPO-1</a></li>
   <li><a href="/master_list/?project=HIPPO-2">HIPPO-2</a></li>
   <li><a href="/master_list/?project=HIPPO-3">HIPPO-3</a></li>
   <li><a href="/master_list/?project=HIPPO-4">HIPPO-4</a></li>
   <li><a href="/master_list/?project=HIPPO-5">HIPPO-5</a></li>
   <li><a href="/master_list/?project=HIWC-FL">HIWC-FL</a></li>
   <li><a href="/master_list/?project=HIWC-RADAR-2018">HIWC-RADAR-2018</a></li>
   <li><a href="/master_list/?project=ICE-L">ICE-L</a></li>
   <li><a href="/master_list/?project=ICE-T">ICE-T</a></li>
   <li><a href="/master_list/?project=ICEBRIDGE">ICEBRIDGE</a></li>
   <li><a href="/master_list/?project=ICEBRIDGE-2015">ICEBRIDGE-2015</a></li>
   <li><a href="/master_list/?project=ICICLE">ICICLE</a></li>
   <li><a href="/master_list/?project=IDEAL">IDEAL</a></li>
   <li><a href="/master_list/?project=IDEAS-1">IDEAS-1</a></li>
   <li><a href="/master_list/?project=IDEAS-2">IDEAS-2</a></li>
   <li><a href="/master_list/?project=IDEAS-3">IDEAS-3</a></li>
   <li><a href="/master_list/?project=IDEAS-4_C130">IDEAS-4 C130</a></li>
   <li><a href="/master_list/?project=IDEAS-4_GV">IDEAS-4 GV</a></li>
   <li><a href="/master_list/?project=IFRACS">IFRACS</a></li>
   <li><a href="/master_list/?project=IHOP_2002">IHOP_2002</a></li>
   <li><a href="/master_list/?project=INDOEX">INDOEX</a></li>
   <li><a href="/master_list/?project=ITEX">ITEX</a></li>
   <li><a href="/master_list/?project=ITOP">ITOP</a></li>
   <li><a href="/master_list/?project=LATTE">LATTE</a></li>
   <li><a href="/master_list/?project=LPB">LPB</a></li>
   <li><a href="/master_list/?project=MASCRAD">MASCRAD</a></li>
   <li><a href="/master_list/?project=MATERHORN-X">MATERHORN-X</a></li>
   <li><a href="/master_list/?project=Meso18-19">Meso18-19</a></li>
   <li><a href="/master_list/?project=MethaneAIR">MethaneAIR</a></li>
   <li><a href="/master_list/?project=MILAGRO">MILAGRO</a></li>
   <li><a href="/master_list/?project=MITTS">MITTS</a></li>
   <li><a href="/master_list/?project=MPEX">MPEX</a></li>
   <li><a href="/master_list/?project=NAME">NAME</a></li>
   <li><a href="/master_list/?project=NOREASTER">NOREASTER</a></li>
   <li><a href="/master_list/?project=OHHI">OHHI</a></li>
   <li><a href="/master_list/?project=ORCAS">ORCAS</a></li>
   <li><a href="/master_list/?project=OTREC">OTREC</a></li>
   <li><a href="/master_list/?project=OTREC-TEST">OTREC-TEST</a></li>
   <li><a href="/master_list/?project=OWLeS">OWLeS</a></li>
   <li><a href="/master_list/?project=PACDEX">PACDEX</a></li>
   <li><a href="/master_list/?project=PACS">PACS</a></li>
   <li><a href="/master_list/?project=PASE">PASE</a></li>
   <li><a href="/master_list/?project=PECAN">PECAN</a></li>
   <li><a href="/master_list/?project=PERDIGAO">PERDIGAO</a></li>
   <li><a href="/master_list/?project=PLATIN">PLATIN</a></li>
   <li><a href="/master_list/?project=PLOWS">PLOWS</a></li>
   <li><a href="/master_list/?project=POST">POST</a></li>
   <li><a href="/master_list/?project=PREDICT">PREDICT</a></li>
   <li><a href="/master_list/?project=RAINEX">RAINEX</a></li>
   <li><a href="/master_list/?project=RELAMPAGO">RELAMPAGO</a></li>
   <li><a href="/master_list/?project=RICO">RICO</a></li>
   <li><a href="/master_list/?project=SALLJEX">SALLJEX</a></li>
   <li><a href="/master_list/?project=SAANGRIA-TEST">SAANGRIA-TEST</a></li>
   <li><a href="/master_list/?project=SABIRPOD">SABIRPOD</a></li>
   <li><a href="/master_list/?project=SAS">SAS</a></li>
   <li><a href="/master_list/?project=SAVANT">SAVANT</a></li>
   <li><a href="/master_list/?project=SMEX">SMEX</a></li>
   <li><a href="/master_list/?project=SNOWIE">SNOWIE</a></li>
   <li><a href="/master_list/?project=SOCRATES">SOCRATES</a></li>
   <li><a href="/master_list/?project=SOCRATES-TEST">SOCRATES-TEST</a></li>
   <li><a href="/master_list/?project=SPICULE">SPICULE</a></li>
   <li><a href="/master_list/?project=SPRITES-II">SPRITES-II</a></li>
   <li><a href="/master_list/?project=START08">START08</a></li>
   <li><a href="/master_list/?project=T-28">T-28</a></li>
   <li><a href="/master_list/?project=T-PARC">T-PARC</a></li>
   <li><a href="/master_list/?project=T-REX">T-REX</a></li>
   <li><a href="/master_list/?project=TCI">TCI</a></li>
   <li><a href="/master_list/?project=TIMREX">TIMREX</a></li>
   <li><a href="/master_list/?project=TORERO">TORERO</a></li>
   <li><a href="/master_list/?project=TORUS_2019">TORUS_2019</a></li>
   <li><a href="/master_list/?project=VERTEX">VERTEX</a></li>
   <li><a href="/master_list/?project=VOCALS">VOCALS</a></li>
   <li><a href="/master_list/?project=VORTEX2">VORTEX2</a></li>
   <li><a href="/master_list/?project=VORTEX-SE_2016">VORTEX-SE_2016</a></li>
   <li><a href="/master_list/?project=VORTEX-SE_2017">VORTEX-SE_2017</a></li>
   <li><a href="/master_list/?project=VORTEX-SE_2018">VORTEX-SE_2018</a></li>
   <li><a href="/master_list/?project=WAMME">WAMME</a></li>
   <li><a href="/master_list/?project=WE-CAN">WE-CAN</a></li>
   <li><a href="/master_list/?project=WE-CAN">WE-CAN-TEST</a></li>
   <li><a href="/master_list/?project=WINTER">WINTER</a></li>
   <li><a href="/master_list/?project=WINTRE-MIX">WINTRE-MIX</a></li>
</ul>

</body>
</html>
   </c:otherwise>
</c:choose>
