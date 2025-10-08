package meta

class DatasetAuthor implements Serializable {

    //static belongsTo = [dataset: Dataset]
	
	static mapping = {
		//table: "dataset_author"
		version false
		id composite: ["dataset", "author"]
	}
	
	Dataset dataset
	Author author
	int sortKey
	
	boolean deleted
	static transients = [ 'deleted' ]

    static constraints = {
		sortKey(blank:false, min:0, max:65534)
		unique:['dataset', 'author']
    }
	
	//String toString() { "Data Set " + dataset?.id + "  -  " + sortKey + ".  " + author }
	
	static DatasetAuthor create(Dataset ds, Author auth, int sort, boolean flush = false) {
		DatasetAuthor dsAuthor = new DatasetAuthor(dataset: ds, author: auth, sortKey: sort)
		dsAuthor.save(flush: flush, insert: true)
		return dsAuthor
	}
	
	static boolean remove(Dataset ds, Author auth, boolean flush = false) {
		DatasetAuthor dsAuthor = DatasetAuthor.findByDatasetAndAuthor(ds, auth)
		return dsAuthor ? dsAuthor.delete(flush: flush) : false
	}
	
	static void removeAll(Dataset ds) {
		executeUpdate("DELETE FROM DatasetAuthor WHERE dataset=:dataset", [dataset: ds])
	}
	
	
	/*
	static DatasetAuthor link(Dataset dataset, Author author, int sortKey) {
		def m = DatasetAuthor.findByDatasetAndAuthor(dataset, author)
		if (!m) {
			m = new DatasetAuthor()
			m.dataset = dataset
			m.author = author
			m.sortKey = sortKey
			m.save()
		}
		return m
	}
	
	static void unlink(Dataset dataset, Author author) {
		def m = DatasetAuthor.findByDatasetAndAuthor(dataset, author)
		if (m) {
			dataset?.removeFromAuthors(m)
			author?.removeFromDatasets(m)
			m.delete()
		}
	}
	*/
}
