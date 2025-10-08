package meta

import java.util.List;

class Frequency {

    //static hasMany = [ datasets: Dataset ]
	
	String name
	int sortKey

	static mapping = {
		datasource 'lookup'
		//datasource [ 'lookup', DEFAULT ]
		version false
		sortKey column: 'sort_key', sqlType: 'smallint unsigned', length: 5
	 }
	
    static constraints = {
		name(maxSize:255)
    }
	
	String toString() {
		return "${name}"
	}
	
	Long getId(Frequency frequency) {
		return frequency.id
	}
	
	// Begin dataset-frequency Functions
	/*
	List datasets() {
		return datasetFrequencies.collect{it.dataset}
	}
	
	List addToDatasetFrequency(Dataset dataset) {
		DatasetFrequency.link(dataset, this)
		return datasets()
	}
	
	List removeFromDatasetFrequency(Dataset dataset) {
		DatasetFrequency.unlink(dataset, this)
	}
	*/
	// End dataset-frequency Functions
}
