package meta



import org.junit.*
import grails.test.mixin.*

@TestFor(DatasetAuthorController)
@Mock(DatasetAuthor)
class DatasetAuthorControllerTests {


    def populateValidParams(params) {
      assert params != null
      // TODO: Populate valid properties like...
      //params["name"] = 'someValidName'
    }

    void testIndex() {
        controller.index()
        assert "/datasetAuthor/list" == response.redirectedUrl
    }

    void testList() {

        def model = controller.list()

        assert model.datasetAuthorInstanceList.size() == 0
        assert model.datasetAuthorInstanceTotal == 0
    }

    void testCreate() {
       def model = controller.create()

       assert model.datasetAuthorInstance != null
    }

    void testSave() {
        controller.save()

        assert model.datasetAuthorInstance != null
        assert view == '/datasetAuthor/create'

        response.reset()

        populateValidParams(params)
        controller.save()

        assert response.redirectedUrl == '/datasetAuthor/show/1'
        assert controller.flash.message != null
        assert DatasetAuthor.count() == 1
    }

    void testShow() {
        controller.show()

        assert flash.message != null
        assert response.redirectedUrl == '/datasetAuthor/list'


        populateValidParams(params)
        def datasetAuthor = new DatasetAuthor(params)

        assert datasetAuthor.save() != null

        params.id = datasetAuthor.id

        def model = controller.show()

        assert model.datasetAuthorInstance == datasetAuthor
    }

    void testEdit() {
        controller.edit()

        assert flash.message != null
        assert response.redirectedUrl == '/datasetAuthor/list'


        populateValidParams(params)
        def datasetAuthor = new DatasetAuthor(params)

        assert datasetAuthor.save() != null

        params.id = datasetAuthor.id

        def model = controller.edit()

        assert model.datasetAuthorInstance == datasetAuthor
    }

    void testUpdate() {
        controller.update()

        assert flash.message != null
        assert response.redirectedUrl == '/datasetAuthor/list'

        response.reset()


        populateValidParams(params)
        def datasetAuthor = new DatasetAuthor(params)

        assert datasetAuthor.save() != null

        // test invalid parameters in update
        params.id = datasetAuthor.id
        //TODO: add invalid values to params object

        controller.update()

        assert view == "/datasetAuthor/edit"
        assert model.datasetAuthorInstance != null

        datasetAuthor.clearErrors()

        populateValidParams(params)
        controller.update()

        assert response.redirectedUrl == "/datasetAuthor/show/$datasetAuthor.id"
        assert flash.message != null

        //test outdated version number
        response.reset()
        datasetAuthor.clearErrors()

        populateValidParams(params)
        params.id = datasetAuthor.id
        params.version = -1
        controller.update()

        assert view == "/datasetAuthor/edit"
        assert model.datasetAuthorInstance != null
        assert model.datasetAuthorInstance.errors.getFieldError('version')
        assert flash.message != null
    }

    void testDelete() {
        controller.delete()
        assert flash.message != null
        assert response.redirectedUrl == '/datasetAuthor/list'

        response.reset()

        populateValidParams(params)
        def datasetAuthor = new DatasetAuthor(params)

        assert datasetAuthor.save() != null
        assert DatasetAuthor.count() == 1

        params.id = datasetAuthor.id

        controller.delete()

        assert DatasetAuthor.count() == 0
        assert DatasetAuthor.get(datasetAuthor.id) == null
        assert response.redirectedUrl == '/datasetAuthor/list'
    }
}
