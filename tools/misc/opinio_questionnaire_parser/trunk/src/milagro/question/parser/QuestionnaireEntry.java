package milagro.question.parser;

import java.util.*;

public class QuestionnaireEntry {

    private Date endDate, startDate;
    private String archive, archiveAlternate, email, entryId, institution, instrument, parameters, piName, site, siteAlternate;
    
    public QuestionnaireEntry(String filename, int id, int matrixLine) {
	entryId = String.format("%s_%d_%d", filename, id, matrixLine);
    }
    
    
    private String cleanQuotes(String data) {
	if (data == null) { return data; }
	
	char[] result = data.toCharArray();
	result[0] = ' ';
	result[result.length - 1] = ' ';
	
	String clean = (new String(result)).trim();
	
	return clean.equals("") ? null : clean;
    }
    
    public String getArchive() { return archive; }
    
    public String getAlternateArchive() { return archiveAlternate; }
    
    public String getAlternateSite() { return siteAlternate; }
    
    public String getEmail() { return email; }
    
    public Date getEndDate() { return endDate; }
    
    public String getId() { return entryId; }
    
    public String getInstitution() { return institution; }
    
    public String getInstrument() { return instrument; }
    
    public String getParameters() { return parameters; }
    
    public String getPI() { return piName; }
    
    public String getSite() { return site; }
    
    public Date getStartDate() { return startDate; }
    
    public void setArchive(String archive, String alternateArchive) {
	this.archive = cleanQuotes(archive);
	this.archiveAlternate = cleanQuotes(alternateArchive);
    }
    
    public void setEmail(String email) {
	this.email = cleanQuotes(email);
    }
    
    public void setInstitution(String institution) {
	this.institution = cleanQuotes(institution);
    }
    
    public void setInstrument(String instrument) {
	this.instrument = cleanQuotes(instrument);
    }
    
    public void setParameters(String params) {
	this.parameters = cleanQuotes(params);
    }
    
    public void setPI(String piName) { this.piName = cleanQuotes(piName); }
    
    public void setSite(String site, String alternateSite) {
	this.site = cleanQuotes(site);
	this.siteAlternate = cleanQuotes(alternateSite);
    }
    
    public void setTime(Date startDate, Date endDate) {
	this.startDate = startDate;
	this.endDate = endDate;
    }
    
    @Override public String toString() {
	StringBuffer sb = new StringBuffer();
	sb.append("ID: ").append(getId()).append("\n");
	sb.append("PI: ").append(getPI()).append("\n");
	sb.append("Institution: ").append(getInstitution()).append("\n");
	sb.append("Email: ").append(getEmail()).append("\n\n");
	
	sb.append("Instrument: ").append(getInstrument()).append("\n");
	sb.append("Site: ").append(getSite()).append("; ").append(getAlternateSite()).append("\n");
	sb.append("Date Range: ").append(String.format("%1$tY/%1$tm/%1$td",getStartDate())).append(" - ").
	    append(String.format("%1$tY/%1$tm/%1$td", getEndDate())).append("\n");
	sb.append("Archive: ").append(getArchive()).append("; ").append(getAlternateArchive()).append("\n\n");
	sb.append("Parameters: ").append(getParameters());
	
	return sb.toString();
    }
}
