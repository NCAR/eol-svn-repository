package dln.beans;

public class XlinkBean {
    
    private int id;
    private String url,purpose,title;

    public XlinkBean() {}

    public XlinkBean(int id, String href, String title, String purpose) {
	setId(id);
	setUrl(href);
	setTitle(title);
	setPurpose(purpose);
    }

    public int getId() { return id; }

    public String getTitle() { return title == null ? "" : title; }

    public String getPurpose() { return purpose; }

    public String getUrl() { return url == null ? "" : url; }

    public void setId(int id) { this.id = id; }

    public void setPurpose(String purpose) { this.purpose = purpose; }

    public void setTitle(String title) { this.title = title; }

    public void setUrl(String url) { this.url = url; }
}
