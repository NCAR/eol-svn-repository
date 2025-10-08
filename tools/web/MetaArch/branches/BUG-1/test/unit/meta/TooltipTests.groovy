package meta



import grails.test.mixin.*
import org.junit.*

/**
 * See the API for {@link grails.test.mixin.domain.DomainClassUnitTestMixin} for usage instructions
 */
@TestFor(Tooltip)
class TooltipTests {

    void testCreate() {
        def tooltipObject = Tooltip.newInstance()

        assert tooltipObject.class == "Tooltip"
    }

    void testUpdate() {
        def tooltipObject = Tooltip.newInstance([])
    }
}
