package meta

class Platform {
	
	//static hasMany = [ datasets: Dataset ]
	
	String name
	String description
	
    static mapping = {
		datasource 'lookup'
		//datasource ([ 'lookup', 'default' ])
		version false
		description type: 'text'
	 }
	
    static constraints = {
		name(maxSize:255)
    }
	
	String toString() {
		return "${name}" // (${description})"
	}
	
	Long getId(Platform platform) {
		return platform.id
	}
	
	// Begin dataset-platform Functions
	/*
	List datasets() {
		return datasetPlatforms.collect{it.dataset}
	}
	
	List addToDatasetPlatform(Dataset dataset) {
		DatasetPlatform.link(dataset, this)
		return datasets()
	}
	
	List removeFromDatasetPlatform(Dataset dataset) {
		DatasetPlatform.unlink(dataset, this)
	}
	*/
	// End dataset-platform Functions
}
