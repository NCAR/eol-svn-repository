<%--
 * The user_titlebar.jsp is a fragment for the user list.  It is the header
 * bar that is displayed at the top and bottom of the page.  It is in a fragment
 * file so updates will be applied to both the top and bottom header.
--%>
<jsp:useBean id="constants" scope="page" class="dln.util.DLNConstants" />

<div class="headerbar">
    <table width=100%>
        <tr>
            <td style="color: #FFFFFF; text-align: left; width: 75%">Users in the ${constants.title}</td>
            <td style="color: #FFFFFF; text-align: right; width: 25%">${param.userCount} Users</td>
        </tr>
    </table>
</div>