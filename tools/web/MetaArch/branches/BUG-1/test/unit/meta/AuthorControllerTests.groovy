package meta



import grails.test.mixin.*
import org.junit.*

/**
 * See the API for {@link grails.test.mixin.web.ControllerUnitTestMixin} for usage instructions
 */
@TestFor(AuthorController)
@Mock(Author)
class AuthorControllerTests {
	
	void testSaveInvalidAuthor() {
       controller.save()
	   
	   assert model.authorInstance != null
	   assert view == '/author/create'
    }
	
	void testSaveValidAuthor() {
		params.firstName = 'Joe'
		params.lastName = 'Bloggs'
		
		controller.save()
		
		assert response.directedUrl == '/author/show/1'
		assert flash.message != null
		assert Author.count() == 1
	}
}
