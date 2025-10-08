package meta;

public enum FileType {
	METADATA("Doc File"),
	DATA("Data File"),
	EVENT_LOG("Event Log");
	
	private final String displayName;
	
	public String toString() { return displayName; }
	public String getKey() { return name(); }
	
	FileType(String displayName) {
		this.displayName = displayName;
	}
}
