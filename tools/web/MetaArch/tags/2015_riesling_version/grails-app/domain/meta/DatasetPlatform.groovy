package meta

class DatasetPlatform {

	/*
	static mapping = {
		datasources(['lookup', 'default'])
		table: 'platform' //, 'dataset'])
	}
	*/
	
	static belongsTo = [dataset: Dataset]
	
	//Dataset dataset
	Long platform
	//Platform platform
	
	boolean deleted
	static transients = [ 'deleted' ]
	
    static constraints = {
		unique:['dataset', 'platform']
    }
	
	static DatasetPlatform link(Dataset dataset, Long platform) {
		def m = DatasetPlatform.findByDatasetAndPlatform(dataset, platform)
		if (!m) {
			m = new DatasetPlatform()
			dataset?.addToDatasetPlatforms(m)
			m.platform = platform
			//platform?.addToDatasetPlatform(m)
			m.save()
		}
		return m
	}
	
	static void unlink(Dataset dataset, Long platform) {
		def m = DatasetPlatform.findByDatasetAndPlatform(dataset, platform)
		if (m)
		{
			dataset?.removeFromDatasetPlatforms(m)
			//platform?.removeFromDatasetPlatform(m)
			m.delete()
		}
	}
}
