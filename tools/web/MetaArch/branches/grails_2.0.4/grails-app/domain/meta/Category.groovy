package meta

import java.util.List;

class Category {
	
	//static hasMany = [ datasets: Dataset ]
	
	String name

	static mapping = {
		datasource 'lookup'
		//datasource [ 'lookup', DEFAULT ]
		version false
	 }
	
    static constraints = {
		name(maxSize:255)
    }
	
	String toString() {
		return "${name}"
	}
	
	Long getId(Category category) {
		return category.id
	}
	
	// Begin dataset-category Functions
	/*
	List datasets() {
		return datasetCategories.collect{it.dataset}
	}
	
	List addToDatasetCategory(Dataset dataset) {
		DatasetCategory.link(dataset, this)
		return datasets()
	}
	
	List removeFromDatasetCategory(Dataset dataset) {
		DatasetCategory.unlink(dataset, this)
	}
	*/
	// End dataset-category Functions
}
