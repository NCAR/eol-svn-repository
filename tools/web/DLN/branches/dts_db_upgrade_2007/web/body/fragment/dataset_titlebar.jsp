<%--
 * The dataset_titlebar.jsp is a fragment for the data set view.  It is the header
 * bar that is displayed at the top and bottom of the page.  It is in a fragment
 * file so updates will be applied to both the top and bottom header.
--%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>

<div class="headerbar">
    <table width=100%>
        <tr>
            <td style="color: #FFFFFF; text-align: left; width: 75%">${param.title}</td>
            <td style="color: #FFFFFF; text-align: right; width: 25%">
                <c:if test="${!empty param.datasetIndex}">
                    ${param.datasetIndex} of
                </c:if>
                ${param.datasetCount} Datasets
            </td>
        </tr>
    </table>
</div>