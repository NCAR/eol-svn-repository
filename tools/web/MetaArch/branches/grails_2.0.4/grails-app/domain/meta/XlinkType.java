package meta;

public enum XlinkType {
	DOWNLOAD("Download"),
	INFO("Information"),
	HOMEPAGE("Homepage"),
	MAP("Map"),
	CATALOG("Catalog"),
	EVENT_LOG("Event Log");
	
	private final String value;
	
	public String toString() { return value; }
	public String getKey() { return name(); } 
	
	XlinkType(String value) {
		this.value = value;
	}
}
