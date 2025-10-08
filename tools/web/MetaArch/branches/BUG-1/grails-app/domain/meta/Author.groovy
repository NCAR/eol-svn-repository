package meta

import java.util.Date;
import java.util.List;

import meta.auth.User;

class Author {
	
	// Created a DatasetAuthor domain with a sort-number to keep the list sortable
	//static hasMany = [datasets: DatasetAuthor]
	
	boolean ajax_added = false
	
	String firstName
	String middleName
	String lastName
	
	String position
	String organization
	
	String email
	String phone
	String fax
	
	String address
	String city
	String state
	String postalCode
	String country
	
	String homepage
	
	Date dateCreated = new Date()
	Date lastUpdated = new Date()

    static constraints = {
		// First and Last names are required
		// Middle name/initial is optional
		firstName(blank: false, nullable: false)
		middleName(blank: true, nullable: true)
		lastName(blank: false, nullable: false)
		
		// Strongly prefer to know the position for this author.
		position(nullable: true)
		organization(blank: false, nullable: false) // Organization is required for all authors.
		
		// At least one is required: e-mail, phone number, address
		email(nullable: true, email: true, unique: true)
		phone(nullable: true, maxSize: 63)
		fax(nullable: true, maxSize: 63)
		address(nullable: true, maxSize:65535)
		city(nullable: true)
		state(nullable: true)
		postalCode(nullable: true, maxSize: 31)
		country(nullable: true, maxSize: 63)
		
		homepage(nullable: true, blank: true, url: true)
    }
	
	String toString() { firstName + " " + lastName + " (" + organization + ")" }
	
	def beforeDelete() {
		// Be sure to delete all author associations!
		DatasetAuthor.removeAll(this)
	}
	
	// Begin dataset-author Functions
	Set<Dataset> datasets() {
		DatasetAuthor.findAllByAuthor(this).collect { it.dataset } as Set
	}
	
	boolean hasDataset(Dataset ds) {
		return DatasetAuthor.countByAuthorAndDataset(this, ds) > 0
	}
	
	/*
	List datasets() {
		return datasets.collect{it.dataset}
	}
	
	List addToDatasetAuthor(Dataset dataset, int sortKey) {
		DatasetAuthor.link(this, dataset, sortKey)
		return datasets()
	}
	/*
	List removeFromDatasetAuthor(Dataset dataset) {
		DatasetAuthor.unlink(this, dataset)
	}*/
	// End dataset-author Functions
}
