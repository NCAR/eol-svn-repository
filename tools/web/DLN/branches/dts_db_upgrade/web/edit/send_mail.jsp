<%----------------------------------------------------------------
 send_mail.jsp: Determines if the user wants to send an e-mail
  when adding a new dataset.  If so, constructs the e-mail and
  sends it using the SMTPBean class.

 Author: Dan Sullivan
 Date: ??
----------------------------------------------------------------%>


<jsp:useBean id="email" scope="page" class="dln.beans.SMTPBean"/>
<%
	String send_mail, from, to, subject, message;
	
	UserBean loader, checker = null;
	ProjectBean project = null;

	send_mail = request.getParameter( "send_mail" );
	if( send_mail.equals( "y" ) )
	{
		loader = UserDBA.getFromDB( dataset.getLoaderId() );
		checker = UserDBA.getFromDB( dataset.getCheckerId() );
		project = new ProjectBean();
		project.setId((String)dataset.getProjects().get(0));

		if( project != null && loader != null && checker != null && 
				loader.getEmail() != null && checker.getEmail() != null &&
				loader.getId() > 2 && checker.getId() > 2 &&
				!loader.getEmail().equals("") && !checker.getEmail().equals("") )
		{
			from = checker.getEmail();
			to = loader.getEmail();
			subject = project.getId() + " Dataset Is Ready To Be Loaded.";

			message = loader.getName() + ",\n\n" + 
				"A dataset has been entered into the data loading notes page for you to load into CODIAC.  " +
				"Below is a summary of what has been entered into the notes page.  For more information " +
				"refer to the data loading notes page at http://www.joss.ucar.edu/dpgapps/dln.\n\n" +
				"Title:  " + dataset.getName() + "\n" +
				"Project: " + project.getId() + "\n" +
				"Person Checking: " + checker.getName() + "\n\n" +
				"Notes:\n" + dataset.getNote();

			email.sendMail(from,to,subject,message);
		}
		else
		{
			System.err.println( "Unable to send e-mail: most probably chose unassigned or Other as a loader or checker" );
		}
	}
%>
