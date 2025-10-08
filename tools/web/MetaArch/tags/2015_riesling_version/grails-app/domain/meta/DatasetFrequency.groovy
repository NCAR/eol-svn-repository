package meta

class DatasetFrequency {

    /*
	static mapping = {
		datasources(['lookup', 'default'])
		table: 'frequency' //, 'dataset'])
	}
	*/
	
	static belongsTo = [dataset: Dataset]
	
	//Dataset dataset
	Long frequency
	//Frequency frequency
	
	boolean deleted
	static transients = [ 'deleted' ]
	
    static constraints = {
		unique:['dataset', 'frequency']
    }
	
	static DatasetFrequency link(Dataset dataset, Long frequency) {
		def m = DatasetFrequency.findByDatasetAndFrequency(dataset, frequency)
		if (!m) {
			m = new DatasetFrequency()
			dataset?.addToDatasetFrequencies(m)
			m.frequency = frequency
			//frequency?.addToDatasetFrequency(m)
			m.save()
		}
		return m
	}
	
	static void unlink(Dataset dataset, Long frequency) {
		def m = DatasetFrequency.findByDatasetAndFrequency(dataset, frequency)
		if (m)
		{
			dataset?.removeFromDatasetFrequencies(m)
			//frequency?.removeFromDatasetFrequency(m)
			m.delete()
		}
	}
}
