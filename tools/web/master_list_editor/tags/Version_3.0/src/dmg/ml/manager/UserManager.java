package dmg.ml.manager;

import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.servlet.http.HttpSession;

/**
 * <p>The UserManager class is a class for managing which user is logged into
 * the Master List, what roles they play, and logging actions.</p>
 *
 * @author Joel Clawson
 */
public class UserManager {

    /**
     * Determine if the current user is a super user that can do all of the
     * functions in the tool.
     * @return <code>true</code> if the user is a super user, <code>false</code>
     * otherwise.
     **/
    public boolean isSuperUser() { 
        return FacesContext.getCurrentInstance().
                    getExternalContext().isUserInRole("super");
    }
    
    /**
     * Logout of the application by invalidating the current session.
     * @param evt The event that triggered the logout action.
     **/
    public void logout(ActionEvent evt) {
        ((HttpSession)FacesContext.getCurrentInstance().getExternalContext().
                getSession(true)).invalidate();
    }
}
