package meta



import grails.test.mixin.*
import org.junit.*

/**
 * See the API for {@link grails.test.mixin.domain.DomainClassUnitTestMixin} for usage instructions
 */
@TestFor(Author)
class AuthorTests {

    void testConstraints() {
		def existingAuthor = new Author(
				firstName: 'Stephen',
				lastName: 'King')
		
		mockForConstraintsTests(Author, [existingAuthor])
		
		// Validations should fail if firstName and lastName are blank
		def author = new Author(
				firstName: '',
				middleName: '',
				lastName: '',
				position: null,
				organization: null,
				email: null,
				phone: null,
				fax: null,
				address: null,
				city: null,
				state: null,
				postalCode: null,
				country: null,
				homepage: null)
		
		assert !author.validate()
		assert "blank" == author.errors["firstName"]
		assert "blank" == author.errors["lastName"]
		
		author.firstName = "Joe"
		author.lastName = "Bloggs"
		
		assert author.validate()
	}
}
