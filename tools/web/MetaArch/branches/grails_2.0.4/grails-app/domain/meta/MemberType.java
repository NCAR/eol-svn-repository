package meta;

public enum MemberType {
	/*
	 * All MemberTypes:
	 * 		Can list/show Projects/Datasets they are connected with.
	 * 
	 * PI:
	 * 		Can edit Projects (not xmlTemplate or CSS), create/edit/delete Datasets
	 * INTERNAL_CONTACT:
	 * 		Can edit Projects, create/edit/delete Datasets
	 * EDITOR:
	 * 		Can create/edit/delete Datasets
	 * MEMBER:
	 * 		Can view Projects/Datasets
	 * MANAGER:
	 * 		Can view Projects/Datasets
	 */
	PI("Principal Investigator"), 
	INTERNAL_CONTACT("Internal Contact"), 
	EDITOR("Editor"), 
	MEMBER("Member"),
	MANAGER("Project Manager");
	
	private final String displayName;
	
	public String toString() { return displayName; }
	
	String getKey() { return name(); }
	
	MemberType(String displayName) {
		this.displayName = displayName;
	}
}
