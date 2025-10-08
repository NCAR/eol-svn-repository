package meta

class DatasetCategory {
	
	/*
	static mapping = {
		datasources(['lookup', 'default'])
	//	table: 'category'  //, 'dataset'])
	}
	*/
	
	static belongsTo = [dataset: Dataset]
	
	//Dataset dataset
	Long category
	//Category category
	
	boolean deleted
	static transients = [ 'deleted' ]

    static constraints = {
		unique:['dataset', 'category']
    }
	
	static DatasetCategory link(Dataset dataset, Long category) {
		def m = DatasetCategory.findByDatasetAndCategory(dataset, category)
		if (!m) {
			m = new DatasetCategory()
			dataset?.addToDatasetCategories(m)
			m.category = category
			//category?.addToDatasetCategory(m)
			m.save()
		}
		return m
	}
	
	static void unlink(Dataset dataset, Long category) {
		def m = DatasetCategory.findByDatasetAndCategory(dataset, category)
		if (m) {
			dataset?.removeFromDatasetCategories(m)
			//m.removeFromDatasetCategories(dataset)
			//category?.removeFromDatasetCategory(m)
			m.delete()
		}
	}
}
