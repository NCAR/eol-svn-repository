package dln.beans;

public class StatusBean {

    private int id;
    private String name;
    private boolean done;

    public StatusBean() {}

    public StatusBean(int id, String name, boolean done) {
	setId(id);
	setName(name);
	setDone(done);
    }

    public int getId() { return id; }

    public String getName() { return name == null ? "" : name; }

    public boolean isDone() { return done; }

    public void setDone(boolean done) { this.done = done; }

    public void setId(int id) { this.id = id; }

    public void setName(String name) { this.name = name; }

    public String toString() { return getName(); }
}
