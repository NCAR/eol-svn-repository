package dln.util;

/******************************************************************************

SMTPException.java: a simple exception used to send e-mails.  As noted below,
this was pulled from the internet.

Dan Sullivan
******************************************************************************/

// ******************************************************************** 
// *  This code is distributed in the hope that it will be useful,    *
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of  *
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            *
// ********************************************************************
//
// Authored 8/3/00 by Jayson Falkner - Jayson@jspinsider.com
//
//   For a comprehensive tutorial as well as the current SMTP
// protocal go to http://www.jspinsider.com/tutorials/insider.html.
//
//////////////////////////////////////////////////////////////////////

public class SMTPException extends Exception {

	private static final long serialVersionUID = -6697633099754271575L;

	/**
	 * Create a new instance of a SMTPException.
	 * @param message The error message for the exception.
	 */
	public SMTPException(String message) { super(message); }
}