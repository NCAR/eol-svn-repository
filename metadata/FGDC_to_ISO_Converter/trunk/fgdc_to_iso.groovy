#!/usr/bin/env groovy
// TODO: Next iteration, add Taxonomic keywords
import groovy.io.FileType

/**
 * This script is intended to convert FGDC XML metadata into ISO 19115-2 metadata
 * Some data may be lost in translation due to conversion from a less descriptive
 * metadata format to a more descriptive one.
 *
 * Functions:
 * - setupCli(): Sets up the command line interface with flags and optional messages
 *
 * - processArgs(): Interprets the flags sent in by the user
 *
 * - getMetadataValue(): Takes a string key and returns a map corresponding to the metadata values found in the FGDC XML
 *     metadata file. It can also be recursively used by sending in "true" as the second value of the
 *     function
 *
 * - recursiveGetMetadataValue(): If "true" was sent in as the second parameter to getMetadataValue(), this function is
 *     used as a helper.
 *
 * - removeAtSignFromEmailAddress(): Strips out the '@' from an email address for security purposes.
 *
 * - insertTaxonomyrecursively(): Translates taxonomic structures from FGDC to ISO format.
 *
 * - createFeatureCatalogFromEainfo(): Creates the feature catalog from the entities and attribute information tag
 *                                      in the FGDC metadata format.
 *
 * - extractFgdc(): Extracts all the core components of FGDC metadata so it can be injected into an ISO template
 *
 * - createAsIso(): Uses Groovy's StreamingMarkupBuilder to build the ISO metadata from the extracted FGDC
 *
 * Note: The script depends on org.apache.commons.lang3.text.WordUtils for word wrapping when the lenght exceeds 80
 * characters
 *
 * @Author Eric Dattore
 * @Date: 7/31/2014
 */

import groovy.transform.Field
import groovy.util.slurpersupport.GPathResult
import groovy.xml.StreamingMarkupBuilder
import groovy.xml.XmlUtil
import org.apache.commons.lang3.text.WordUtils

// @Field required for global scope
@Field GPathResult reader
@Field output_dir = null
@Field file = null
@Field directory = null
@Field recursive = null
@Field cli
@Field static final String CODE_LIST = 'http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml'

// Each of the ten distinct FGDC sections
@Field idinfo = [:]
@Field dataqual = [:]
@Field spdoinfo = [:]
@Field spref = [:]
@Field eainfo = [:]
@Field distinfo = [:]
@Field metainfo = [:]
@Field citeinfo = [:]
@Field timeinfo = [:]
@Field cntinfo = [:]
@Field taxon = [:]

/**
 * Sets up the command line interface with options
 *
 * @return
 */
def setupCli() {
    cli = new CliBuilder(
            usage: 'fgdc_to_iso.groovy -[hdro]',
            header: '\nAvailable options (use -h or --help for help):\n',
            footer: '\nArguments provided via the required options is used for converting FGDC metadata to ISO ' +
                    'formatted metadata'
    )

    cli.with {
        h longOpt: 'help', 'Show usage information', required: false
        d longOpt: 'directory', 'Specifies the given directory that the specified file is located in',
                args: 1, argName: 'directory', required: false
        r longOpt: 'recursive', 'Recursively process the given directory, requires the directory option',
                required: false
        o longOpt: 'out-dir', 'Specifies the output directory you wish to place the converted file in.' +
                'If no option present, the file is placed in the current working directory', args: 1,
                argName: 'output directory', required: false
    }

    return cli.parse(args)
}

/**
 * Parses the options sent in by the user. Changes behavior based on which flags are sent in.
 *
 * @param options
 * @return
 */
def processArgs(OptionAccessor options) {
    if (!options || args.length < 1) {
        println 'No file specified. Please use -h or --help to view help details'
        return
    }

    if (options.h) {
        cli.usage()
        System.exit(0)
    }

    if (options.d) {
        directory = options.d
    }

    if (options.r) {
        recursive = true
        if (!directory) {
            println 'Error! You cannot use the recursive option without specifying a directory. Exiting...'
            System.exit(1);
        } else {
            file = []
            def pattern = ~/\.xml/
            directory = directory.replaceFirst(/[\/]*\s*$/, '')
            def dir = new File(directory)
            // Iterate through all directories searching for more directories
            dir.eachFileRecurse(FileType.FILES) { filename ->
                if (filename =~ pattern) {
                    file << filename
                }
            }
        }
    }

    if (options.o) {
        output_dir = options.o
    }

    if (args.length >= 1 && !options.r) {
        if (directory) {
            file = directory + options.arguments()[options.arguments().size() - 1]
        } else {
            file = options.arguments()[options.arguments().size() - 1]
        }
    }

    if (file == null) {
        println 'No file name was supplied. Exiting...'
        System.exit(0)
    }
}

/**
 * Recursively searches the XML document for the given item specified by a string.
 * If true is sent in as the second parameter, it will search recursively through the XML document.
 *
 * @param item
 * @param recursive
 * @return
 */
def getMetadataValue(String item, recursive = false) {
    retVal = reader.depthFirst().findAll { it.name() == item }

    if (recursive && retVal.size() > 0) {
        if (retVal.size() > 1) {
            returnMap = []
            for (val in retVal) {
                returnMap += recursiveGetMetadataValue(val)
            }

            return returnMap
        } else
            return recursiveGetMetadataValue(retVal)
    } else {
        if (retVal.size() > 1)
            return retVal
        else if (retVal.size() == 1)
            return retVal
        else
            return null
    }
}

/**
 * Helper function to recursively search the XML document. Returns a map if identical elements are found.
 *
 * @param retVal
 * @param map
 * @return
 */
def recursiveGetMetadataValue(retVal, map = new LinkedHashMap()) {
    retVal.each { node ->
        if (!node.children().isEmpty()) {
            for (item in node.children()) {
                recursiveGetMetadataValue(item, map)
            }
        } else {
            if (map[node.name()])
                if (map[node.name()] instanceof ArrayList) {
                    map[node.name()].add(node.text())
                } else {
                    map[node.name()] = [map[node.name()], node.text()]
                }
            else
                map.put(node.name(), node.text())
        }
    }

    return map
}

/**
 * Simple helper function to strip the '@' from an email address when placing email addresses into ISO XML.
 *
 * @param email
 * @return
 */
def removeAtSignFromEmailAddress(email) {
    return email.replaceFirst('@', ' at ').toString()
}

/**
 * Inserts taxonomies extracted from FGDC in a recursive fashion (based on the FGDC Biological Data Profile).
 *
 * @param nodes
 * @return
 */
def insertTaxonomyRecursively(nodes) {
    def str = ''
    for (node in nodes) {
        if (!node.children().isEmpty()) {
            str += '''\'gmd:taxonCl\' {\n\t\'gmd:MD_TaxonCl\' {\n\t''' + insertTaxonomyRecursively(node.children()) + '''\n}\n}\n'''
        } else {
            if (node.name() == 'common') {
                str = new StringBuffer(str).insert(0, "'gmd:${node.name()}' " +
                        "{\n\t'gco:CharacterString'(\"${node.text()}\")\n}\n").toString()
            } else {
                str += "'gmd:${node.name()}' {\n\t'gco:CharacterString'('${node.text()}')\n}\n"
            }
        }
    }
    return str
}

/**
 * Creates the Feature Catalogue that corresponds to the entity and attribute type (section 5) in FGDC
 *
 * @return
 */
def createFeatureCatalogFromEainfo(file) {
    def catalogBuilder = new StreamingMarkupBuilder()
    def catalogOut = catalogBuilder.bind {
        mkp.xmlDeclaration()
        mkp.declareNamespace('gco':'http://www.isotc211.org/2005/gco')
        mkp.declareNamespace('gfc':'http://www.isotc211.org/2005/gfc')
        mkp.declareNamespace('gmd':'http://www.isotc211.org/2005/gmd')
        mkp.declareNamespace('gmx':'http://www.isotc211.org/2005/gmx')
        mkp.declareNamespace('gsr':'http://www.isotc211.org/2005/gsr')
        mkp.declareNamespace('gss':'http://www.isotc211.org/2005/gss')
        mkp.declareNamespace('gts':'http://www.isotc211.org/2005/gts')
        mkp.declareNamespace('gml':'http://www.opengis.net/gml/3.2')
        mkp.declareNamespace('xsi':'http://www.w3.org/2001/XMLSchema-instance')
        'gfc:FC_FeatureCatalogue'('xmlns':'http://www.isotc211.org/2005/gfc',
                'xsi:schemaLocation':'http://www.isotc211.org/2005/gfc http://www.ngdc.noaa.gov/metadata/published/' +
                        'xsd/schema/gfc/gfc.xsd', 'id':'FC001') {
            'gmx:name' {
                'gco:CharacterString'('Feature Catalogue for ' + idinfo['title'])
            }
            'gmx:scope'('gco:nilReason': 'unknown')
            'gmx:versionNumber'('gco:nilReason': 'unknown')
            'gmx:versionDate'('gco:nilReason': 'inapplicable')
            'gmx:language' {
                'gco:CharacterString'('eng; USA')
            }
            'gmx:characterSet' {
                'gmd:MD_CharacterSetCode'(codeList: CODE_LIST + '#MD_CharacterSetCode', codeListValue: 'utf8')
            }
            'gfc:producer' {
                'gmd:CI_ResponsibleParty' {
                    'gmd:individualName' {
                        'gco:CharacterString'(metainfo['cntper'])
                    }
                    'gmd:organisationName' {
                        'gco:CharacterString'(metainfo['cntorg'])
                    }
                    'gmd:positionName' {
                        'gco:CharacterString'(metainfo['cntpos'])
                    }
                    'gmd:contactInfo' {
                        'gmd:CI_Contact' {
                            'gmd:phone' {
                                'gmd:CI_Telephone' {
                                    'gmd:voice' {
                                        'gco:CharacterString'(metainfo['cntvoice'])
                                    }
                                    'gmd:facsimile' {
                                        'gco:CharacterString'(metainfo['cntfax'])
                                    }
                                }
                            }
                            'gmd:address' {
                                'gmd:CI_Address' {
                                    'gmd:deliveryPoint' {
                                        'gco:CharacterString'(metainfo['address'])
                                    }
                                    'gmd:city' {
                                        'gco:CharacterString'(metainfo['city'])
                                    }
                                    'gmd:administrativeArea' {
                                        'gco:CharacterString'(metainfo['state'])
                                    }
                                    'gmd:postalCode' {
                                        'gco:CharacterString'(metainfo['postal'])
                                    }
                                    'gmd:country' {
                                        'gco:CharacterString'(metainfo['country'])
                                    }
                                    if (metainfo['cntemail'] instanceof ArrayList && metainfo['cntemail'].size() > 1) {
                                        for (email in metainfo['cntemail']) {
                                            'gmd:electronicMailAddress' {
                                                'gco:CharacterString'(removeAtSignFromEmailAddress(email))
                                            }
                                        }
                                    } else {
                                        'gmd:electronicMailAddress' {
                                            'gco:CharacterString'(removeAtSignFromEmailAddress(metainfo['cntemail']))
                                        }
                                    }
                                }
                            }
                        }
                    }
                    'gmd:role' {
                        'gmd:CI_RoleCode'(codeList: CODE_LIST + '#CI_RoleCode', codeListValue: 'resourceProvider',
                                'resourceProvider')
                    }
                }
            }

            if (eainfo instanceof ArrayList) {
                for (featureType in eainfo) {
                    if (featureType['enttypl'] && featureType['enttypd']) {
                        'gfc:featureType' {
                            'gfc:FC_FeatureType' {
                                'gfc:typeName' {
                                    'gco:LocalName'(featureType['enttypl'])
                                }
                                'gfc:definition' {
                                    'gco:CharacterString'(featureType['enttypd'])
                                }
                                'gfc:isAbstract' {
                                    'gco:Boolean'('false')
                                }
                                'gfc:featureCatalogue'(uuidref: 'FC001')
                                for (def i in 0..featureType['attrlabl'].size()) {
                                    'gfc:carrierOfCharacteristics' {
                                        'gfc:FC_FeatureAttribute' {
                                            'gfc:memberName' {
                                                'gco:LocalName'(featureType['attrlabl'][i])
                                            }
                                            'gfc:definition' {
                                                'gco:CharacterString'(featureType['attrdef'][i])
                                            }
                                            'gfc:cardinality'('gco:nilReason': 'unknown')
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                'gfc:featureType' {
                    'gfc:FC_FeatureType' {
                        'gfc:typeName' {
                            'gco:LocalName'(eainfo['enttypl'])
                        }
                        'gfc:definition' {
                            'gco:CharacterString'(eainfo['enttypd'])
                        }
                        'gfc:isAbstract' {
                            'gco:Boolean'('false')
                        }
                        'gfc:featureCatalogue'(uuidref: 'FC001')
                        if (eainfo['attrlabl']) {
                            for (def i in 0..eainfo['attrlabl'].size()) {
                                'gfc:carrierOfCharacteristics' {
                                    'gfc:FC_FeatureAttribute' {
                                        'gfc:memberName' {
                                            'gco:LocalName'(eainfo['attrlabl'][i])
                                        }
                                        'gfc:definition' {
                                            'gco:CharacterString'(eainfo['attrdef'][i])
                                        }
                                        'gfc:cardinality'('gco:nilReason': 'unknown')
                                    }
                                }
                            }
                        } else {
                            'gfc:carrierOfCharacteristics' {
                                'gfc:FC_FeatureAttribute' {
                                    'gfc:memberName' {
                                        'gco:LocalName'(eainfo['attrlabl'])
                                    }
                                    'gfc:definition' {
                                        'gco:CharacterString'(eainfo['attrdef'])
                                    }
                                    'gfc:cardinality'('gco:nilReason': 'unknown')
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    def catalogXml = XmlUtil.serialize(catalogOut)
    println 'Section 5 of FGDC has been found and the feature catalog has been created.'

    if (file.lastIndexOf('/') > -1) {
        file = file.substring(file.lastIndexOf('/'))
    }

    if (file.lastIndexOf('.') > -1) {
        file = file.substring(0, file.lastIndexOf('.'))
    }

    file += '_feature_catalog.xml'

    if (output_dir) {
        def fileDir = new File(output_dir)
        if (!fileDir.exists()) {
            fileDir.mkdirs()
        }

        new File(fileDir, file).write(catalogXml, 'UTF-8')
    } else {
        new File('./' + file).write(catalogXml, 'UTF-8')
    }

    return file
}

/**
 * Extracts most relevant FGDC information from the XML input.
 *
 * @return
 */
def extractFgdc() {
    idinfo = getMetadataValue('idinfo', true)
    dataqual = getMetadataValue('dataqual', true)
    spdoinfo = getMetadataValue('spdoinfo', true)
    spref = getMetadataValue('spref', true)
    eainfo = getMetadataValue('detailed', true)
    distinfo = getMetadataValue('distinfo', true)
    metainfo = getMetadataValue('metainfo', true)
    citeinfo = getMetadataValue('citeinfo', true)
    timeinfo = getMetadataValue('timeinfo', true)
    cntinfo = getMetadataValue('cntinfo', true)
    taxon = getMetadataValue('taxonomy', true)
}

/**
 * Creates ISO 19115-2 metadata from the information extracted from the FGDC formatted input file.
 *
 * @return
 */
def createAsIso(file) {
    println 'Converting ' + file + ' to ISO 19115-2. Please wait...'

    def builder = new StreamingMarkupBuilder()
    def schema = (taxon != null) ? 'ftp://ftp.ncddc.noaa' +
            '.gov/pub/Metadata/Online_ISO_Training/Intro_to_ISO/schemas/ISObio/schema.xsd' : 'http://ngdc.noaa' +
            '.gov/metadata/published/xsd/schema.xsd'

    def file_ident = ''

    if (eainfo) {
        file_ident = createFeatureCatalogFromEainfo(file)
    }

    def out = builder.bind {
        mkp.xmlDeclaration()
        mkp.declareNamespace(gco:'http://www.isotc211.org/2005/gco')
        mkp.declareNamespace(gmd:'http://www.isotc211.org/2005/gmd')
        mkp.declareNamespace(gmi:'http://www.isotc211.org/2005/gmi')
        mkp.declareNamespace(gmx:'http://www.isotc211.org/2005/gmx')
        mkp.declareNamespace(gsr:'http://www.isotc211.org/2005/gsr')
        mkp.declareNamespace(gss:'http://www.isotc211.org/2005/gss')
        mkp.declareNamespace(gts:'http://www.isotc211.org/2005/gts')
        mkp.declareNamespace(gml:'http://www.opengis.net/gml/3.2')
        mkp.declareNamespace(xlink:'http://www.w3.org/1999/xlink')
        mkp.declareNamespace(xsi:'http://www.w3.org/2001/XMLSchema-instance')
        'gmi:MI_Metadata'(xmlns:'http://www.isotc211.org/2005/gmi', 'xsi:schemaLocation': 'http://www.isotc211' +
                '.org/2005/gmi ' + schema) {
            'gmd:fileIdentifier' {
                'gco:CharacterString'(file.substring(file.lastIndexOf('/') + 1,
                        file.lastIndexOf('.')) + '_iso.xml')
            }
            'gmd:language' {
                'gco:CharacterString'('eng; USA')
            }
            'gmd:characterSet' {
                'gmd:MD_CharacterSetCode'(codeList: CODE_LIST + '#MD_CharacterSetCode', codeListValue: 'utf-8', 'utf8')
            }
            'gmd:hierarchyLevel' {
                'gmd:MD_ScopeCode'(codeList: CODE_LIST + '#MD_ScopeCode', codeListValue: 'dataset', 'dataset')
            }
            'gmd:contact' {
                'gmd:CI_ResponsibleParty' {
                    if (metainfo['cntper'] != null) {
                        'gmd:individualName' {
                            'gco:CharacterString'(metainfo['cntper'])
                        }
                    }

                    if (metainfo['cntorg'] != null) {
                        'gmd:organisationName' {
                            'gco:CharacterString'(metainfo['cntorg'])
                        }
                    }

                    'gmd:contactInfo' {
                        'gmd:CI_Contact' {
                            if (metainfo['cntvoice']) {
                                'gmd:phone' {
                                    'gmd:CI_Telephone' {

                                        'gmd:voice' {
                                            'gco:CharacterString'(metainfo['cntvoice'])
                                        }
                                    }
                                }
                            }

                            if (metainfo['addrtype']) {
                                'gmd:address' {
                                    'gmd:CI_Address' {
                                        if (metainfo['address']) {
                                            'gmd:deliveryPoint' {
                                                'gco:CharacterString'(metainfo['address'])
                                            }
                                        }

                                        if (metainfo['city']) {
                                            'gmd:city' {
                                                'gco:CharacterString'(metainfo['city'])
                                            }
                                        }

                                        if (metainfo['state']) {
                                            'gmd:administrativeArea' {
                                                'gco:CharacterString'(metainfo['state'])
                                            }
                                        }

                                        if (metainfo['postal']) {
                                            'gmd:postalCode' {
                                                'gco:CharacterString'(metainfo['postal'])
                                            }
                                        }

                                        if (metainfo['country']) {
                                            'gmd:country' {
                                                'gco:CharacterString'(metainfo['country'])
                                            }
                                        }

                                        if (metainfo['cntemail']) {
                                            if (metainfo['cntemail'] instanceof ArrayList) {
                                                for (def email in metainfo['cntemail']) {
                                                    if (!email.contains('codiac')) {
                                                        'gmd:electronicMailAddress' {
                                                            'gco:CharacterString'(removeAtSignFromEmailAddress(email))
                                                        }
                                                    }
                                                }
                                            } else {
                                                'gmd:electronicMailAddress' {
                                                    'gco:CharacterString'(removeAtSignFromEmailAddress
                                                            (metainfo['cntemail']))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    'gmd:role' {
                        'gmd:CI_RoleCode'(codeList: CODE_LIST + '#CI_RoleCode', codeListValue: 'pointOfContact',
                                'pointOfContact')
                    }
                }
            }
            if (metainfo['metd']) {
                'gmd:dateStamp' {
                    'gco:Date'(metainfo['metd'])
                }
            }
            'gmd:metadataStandardName' {
                'gco:CharacterString'('ISO 19115-2 Geographic Information - Metadata - Part 2: Extensions for Imagery' +
                        ' and Gridded Data')
            }
            'gmd:metadataStandardVersion' {
                'gco:CharacterString'('ISO 19115-2:2009(E)')
            }
            if (taxon) {
                'gmd:metadataExtensionInfo' {
                    'gmd:MD_MetadataExtensionInformation' {
                        'gmd:extensionOnLineResource'()
                        'gmd:extendedElementInformation' {
                            'gmd:MD_ExtendedElementInformation' {
                                'gmd:name' {
                                    'gco:CharacterString'('Taxonomy System')
                                }
                                'gmd:shortName' {
                                    'gco:CharacterString'('Taxonsys')
                                }
                                'gmd:definition' {
                                    'gco:CharacterString'('Documentation of taxonomic sources, procedures, ' +
                                            'and treatments.')
                                }
                                'gmd:obligation' {
                                    'gmd:MD_ObligationCode'('optional')
                                }
                                'gmd:dataType' {
                                    'gmd:MD_DatatypeCode'(codeList: CODE_LIST + '#MD_DatatypeCode',
                                            codeListValue: 'class', 'class')
                                }
                                'gmd:maximumOccurrence' {
                                    'gco:CharacterString'('1')
                                }
                                'gmd:parentEntity' {
                                    'gco:CharacterString'('MD_Identification')
                                }
                                'gmd:rule' {
                                    'gco:CharacterString'('New Metadata section as a class to MD_Identification')
                                }
                                'gmd:rationale' {
                                    'gco:CharacterString'('The set of data elements contained within the class ' +
                                            'element represents an attempt to provide better documentation of ' +
                                            'taxonomic sources, procedures, and treatments.')
                                }
                                'gmd:source' {
                                    'gmd:CI_ResponsibleParty' {
                                        'gmd:organisationName' {
                                            'gco:CharacterString'('National Biological Information Infrastrucure ' +
                                                    '(NBII)')
                                        }
                                        'gmd:role' {
                                            'gmd:CI_RoleCode'(codeList: CODE_LIST + '#CI_RoleCode',
                                                    codeListValue: 'resourceProvider', 'resourceProvider')
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            'gmd:identificationInfo' {
                'gmd:MD_DataIdentification' {
                    'gmd:citation' {
                        'gmd:CI_Citation' {
                            if (idinfo['title']) {
                                'gmd:title' {
                                    'gco:CharacterString'(idinfo['title'])
                                }
                            }
                            'gmd:date' {
                                'gmd:CI_Date' {
                                    if (idinfo['pubdate'] instanceof ArrayList) {
                                        if (idinfo['pubdate'][0].toLowerCase().contains('unpublished') ||
                                                idinfo['pubdate'][0].toLowerCase().contains('unknown')) {
                                            'gmd:date'('gco:nilReason': 'unpublished material')
                                            'gmd:dateType' {
                                                'gmd:CI_DateTypeCode'(codeList: CODE_LIST + '#CI_DateTypeCode',
                                                        codeListValue: 'publication', 'publication')
                                            }
                                        } else if (true) {
                                            'gmd:date'(idinfo['pubdate'][0])
                                            'gmd:dateType' {
                                                'gmd:CI_DateTypeCode'(codeList: CODE_LIST + '#CI_DateTypeCode',
                                                        codeListValue: 'publication', 'publication')
                                            }
                                        }
                                    } else {
                                        if (idinfo['pubdate'].toLowerCase().contains('unpublished') ||
                                                idinfo['pubdate'].toLowerCase().contains('unknown')) {
                                            'gmd:date'('gco:nilReason': 'unpublished material')
                                            'gmd:dateType' {
                                                'gmd:CI_DateTypeCode'(codeList: CODE_LIST + '#CI_DateTypeCode',
                                                        codeListValue: 'publication', 'publication')
                                            }
                                        } else if (true) {
                                            'gmd:date' {
                                                'gco:Date'(idinfo['pubdate'])
                                            }
                                            'gmd:dateType' {
                                                'gmd:CI_DateTypeCode'(codeList: CODE_LIST + '#CI_DateTypeCode',
                                                        codeListValue: 'publication', 'publication')
                                            }
                                        }
                                    }
                                }
                            }
                            if (idinfo['origin'] instanceof ArrayList && idinfo['origin'].size() > 1) {
                                for (originator in idinfo['origin']) {
                                    'gmd:citedResponsibleParty' {
                                        'gmd:CI_ResponsibleParty' {
                                            'gmd:individualName' {
                                                'gco:CharacterString'(originator)
                                            }
                                            'gmd:role' {
                                                'gmd:CI_RoleCode'(codeList: 'http://www.isotc211' +
                                                        '.org/2005/resources/Codelists/gmxCodelists.xml#CI_RoleCode',
                                                        codeListValue: 'originator', 'originator')
                                            }
                                        }
                                    }
                                }
                            } else {
                                'gmd:citedResponsibleParty' {
                                    'gmd:CI_ResponsibleParty' {
                                        'gmd:individualName' {
                                            'gco:CharacterString'(idinfo['origin'])
                                        }
                                        'gmd:role' {
                                            'gmd:CI_RoleCode'(codeList: 'http://www.isotc211' +
                                                    '.org/2005/resources/Codelists/gmxCodelists.xml#CI_RoleCode',
                                                    codeListValue: 'originator', 'originator')
                                        }
                                    }
                                }
                            }
                        }
                    }
                    'gmd:abstract' {
                        'gco:CharacterString'(WordUtils.wrap(idinfo['abstract'], 80))
                    }
                    'gmd:purpose' {
                        'gco:CharacterString'(WordUtils.wrap(idinfo['purpose'], 80))
                    }
                    'gmd:status' {
                        def code
                        if (idinfo['progress'].toLowerCase().contains('work')) {
                            code = 'underDevelopment'
                        } else if (idinfo['progress'].toLowerCase().contains('complete')) {
                            code = 'complete'
                        } else if (idinfo['progress'].toLowerCase().contains('planned')) {
                            code = 'planned'
                        }
                        'gmd:MD_ProgressCode'(codeList: CODE_LIST + '#MD_ProgressCode', codeListValue: code, code)
                    }
                    if (idinfo['cntper']) {
                        'gmd:pointOfContact' {
                            'gmd:CI_ResponsibleParty' {
                                'gmd:individualName' {
                                    'gco:CharacterString'(idinfo['cntper'])
                                }
                                'gmd:organisationName' {
                                    'gco:CharacterString'(idinfo['cntorg'])
                                }
                                'gmd:contactInfo' {
                                    'gmd:CI_Contact' {
                                        'gmd:phone' {
                                            'gmd:CI_Telephone' {
                                                'gmd:voice' {
                                                    'gco:CharacterString'(idinfo['cntvoice'])
                                                }
                                                if (idinfo['cntfax']) {
                                                    'gmd:facsimile' {
                                                        'gco:CharacterString'(idinfo['cntfax'])
                                                    }
                                                }
                                            }
                                        }
                                        'gmd:address' {
                                            'gmd:CI_Address' {
                                                'gmd:deliveryPoint' {
                                                    'gco:CharacterString'(idinfo['address'])
                                                }
                                                'gmd:city' {
                                                    'gco:CharacterString'(idinfo['city'])
                                                }
                                                'gmd:administrativeArea' {
                                                    'gco:CharacterString'(idinfo['state'])
                                                }
                                                'gmd:postalCode' {
                                                    'gco:CharacterString'(idinfo['postal'])
                                                }
                                                'gmd:country' {
                                                    'gco:CharacterString'(idinfo['country'])
                                                }
                                                if (idinfo['cntemail'] instanceof ArrayList) {
                                                    for (def email in idinfo['cntemail']) {
                                                        if (!email.contains('codiac')) {
                                                            'gmd:electronicMailAddress' {
                                                                'gco:CharacterString'(removeAtSignFromEmailAddress(email))
                                                            }
                                                        }
                                                    }
                                                } else {
                                                    if (idinfo['cntemail']) {
                                                        'gmd:electronicMailAddress' {
                                                            'gco:CharacterString'(removeAtSignFromEmailAddress
                                                                    (idinfo['cntemail']))
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        'gmd:contactInstructions' {
                                            'gco:CharacterString'(idinfo['cntinst'])
                                        }
                                    }
                                }
                                'gmd:role' {
                                    'gmd:CI_RoleCode'(codeList: CODE_LIST + '#CI_RoleCode',
                                            codeListValue: 'pointOfContact', 'pointOfContact')
                                }
                            }
                        }
                    }
                    'gmd:resourceMaintenance' {
                        'gmd:MD_MaintenanceInformation' {
                            'gmd:maintenanceAndUpdateFrequency' {
                                def value
                                if (idinfo['update'].toLowerCase().contains('continually')) {
                                    value = 'continual'
                                } else if (idinfo['update'].toLowerCase().contains('daily')) {
                                    value = 'daily'
                                } else if (idinfo['update'].toLowerCase().contains('weekly')) {
                                    value = 'weekly'
                                } else if (idinfo['update'].toLowerCase().contains('monthly')) {
                                    value = 'monthly'
                                } else if (idinfo['update'].toLowerCase().contains('annually')) {
                                    value = 'annually'
                                } else if (idinfo['update'].toLowerCase().contains('unknown')) {
                                    value = 'unknown'
                                } else if (idinfo['update'].toLowerCase().contains('as needed')) {
                                    value = 'asNeeded'
                                } else if (idinfo['update'].toLowerCase().contains('irregular')) {
                                    value = 'irregular'
                                } else if (idinfo['update'].toLowerCase().contains('none planned')) {
                                    value = 'notPlanned'
                                } else {
                                    value = 'unknown'
                                }
                                'gmd:MD_MaintenanceFrequencyCode'(codeList: CODE_LIST +
                                        '#MD_MaintenanceFrequencyCode', codeListValue: value, value)
                            }
                        }
                    }
                    def themesCollection = getMetadataValue('theme', true)
                    if (themesCollection && themesCollection instanceof ArrayList && themesCollection.size() > 1) {
                        for (themeCollection in themesCollection) {
                            'gmd:descriptiveKeywords' {
                                'gmd:MD_Keywords' {
                                    if (themeCollection['themekey'] instanceof ArrayList && themeCollection['themekey'].size() > 1) {
                                        for (theme in themeCollection['themekey']) {
                                            'gmd:keyword' {
                                                'gco:CharacterString'(theme)
                                            }
                                        }
                                    } else {
                                        'gmd:keyword' {
                                            'gco:CharacterString'(themeCollection['themekey'])
                                        }
                                    }
                                    'gmd:type' {
                                        'gmd:MD_KeywordTypeCode'(codeList: CODE_LIST + '#MD_KeywordTypeCode',
                                                codeListValue: 'theme', 'theme')
                                    }
                                    'gmd:thesaurusName' {
                                        'gmd:CI_Citation' {
                                            'gmd:title' {
                                                'gco:CharacterString'(themeCollection['themekt'])
                                            }
                                            'gmd:date'('gco:nilReason': 'unknown')
                                        }
                                    }
                                }
                            }
                        }
                    } else if (themesCollection) {
                        'gmd:descriptiveKeywords' {
                            'gmd:MD_Keywords' {
                                if (themesCollection['themekey'] instanceof ArrayList && themesCollection['themekey']
                                        .size() > 1) {
                                    for (theme in themesCollection['themekey']) {
                                        'gmd:keyword' {
                                            'gco:CharacterString'(theme)
                                        }
                                    }
                                } else {
                                    'gmd:keyword' {
                                        'gco:CharacterString'(themesCollection['themekey'])
                                    }
                                }
                                'gmd:type' {
                                    'gmd:MD_KeywordTypeCode'(codeList: CODE_LIST + '#MD_KeywordTypeCode',
                                            codeListValue: 'theme', 'theme')
                                }
                                'gmd:thesaurusName' {
                                    'gmd:CI_Citation' {
                                        'gmd:title' {
                                            'gco:CharacterString'(themesCollection['themekt'])
                                        }
                                        'gmd:date'('gco:nilReason': 'unknown')
                                    }
                                }
                            }
                        }
                    }
                    def placesCollection = getMetadataValue('place', true)
                    if (placesCollection && placesCollection instanceof ArrayList && placesCollection.size() > 1) {
                        for (placeCollection in placesCollection) {
                            'gmd:descriptiveKeywords' {
                                'gmd:MD_Keywords' {
                                    if (placeCollection['placekey'] instanceof ArrayList &&
                                            placeCollection['placekey'].size() > 1) {
                                        for (place in placeCollection['placekey']) {
                                            'gmd:keyword' {
                                                'gco:CharacterString'(place)
                                            }
                                        }
                                    } else {
                                        'gmd:keyword' {
                                            'gco:CharacterString'(placeCollection['placekey'])
                                        }
                                    }
                                    'gmd:type' {
                                        'gmd:MD_KeywordTypeCode'(codeList: CODE_LIST + '#MD_KeywordTypeCode',
                                                codeListValue: 'place', 'place')
                                    }
                                    'gmd:thesaurusName' {
                                        'gmd:CI_Citation' {
                                            'gmd:title' {
                                                'gco:CharacterString'(placeCollection['placekt'])
                                            }
                                            'gmd:date'('gco:nilReason': 'unknown')
                                        }
                                    }
                                }
                            }
                        }
                    } else if (placesCollection) {
                        'gmd:descriptiveKeywords' {
                            'gmd:MD_Keywords' {
                                if (placesCollection['placekey'] instanceof ArrayList &&
                                        placesCollection['placekey'].size() > 1) {
                                    for (place in placesCollection['placekey']) {
                                        'gmd:keyword' {
                                            'gco:CharacterString'(place)
                                        }
                                    }
                                } else {
                                    'gmd:keyword' {
                                        'gco:CharacterString'(placesCollection['placekey'])
                                    }
                                }
                                'gmd:type' {
                                    'gmd:MD_KeywordTypeCode'(codeList: CODE_LIST + '#MD_KeywordTypeCode',
                                            codeListValue: 'place', 'place')
                                }
                                'gmd:thesaurusName' {
                                    'gmd:CI_Citation' {
                                        'gmd:title' {
                                            'gco:CharacterString'(placesCollection['placekt'])
                                        }
                                        'gmd:date'('gco:nilReason': 'unknown')
                                    }
                                }
                            }
                        }
                    }
                    'gmd:resourceConstraints' {
                        'gmd:MD_LegalConstraints' {
                            'gmd:accessConstraints' {
                                'gmd:MD_RestrictionCode'(codeList: CODE_LIST + '#MD_RestrictionCode',
                                        codeListValue: 'otherRestrictions', 'otherRestrictions')
                            }
                            'gmd:useConstraints' {
                                'gmd:MD_RestrictionCode'(codeList: CODE_LIST + '#MD_RestrictionCode',
                                        codeListValue: 'otherRestrictions', 'otherRestrictions')
                            }
                            'gmd:otherConstraints' {
                                def constraints = ''
                                if (idinfo && idinfo['accconst']) {
                                    constraints += 'Access Constraints: ' + idinfo['accconst']
                                }

                                if (idinfo && idinfo['useconst']) {
                                    constraints += ' Use Constraints: ' + idinfo['useconst']
                                }

                                if (distinfo && distinfo['distliab']) {
                                    constraints += ' Distribution Liability: ' + distinfo['distliab']
                                }
                                'gco:CharacterString'(WordUtils.wrap(constraints, 80))
                            }
                        }
                    }
                    if (taxon) {
                        'gmd:taxonomy' {
                            'gmd:MD_TaxonSys' {
                                def taxonsys = getMetadataValue('classsys', true)
                                if (taxonsys) {
                                    if (taxonsys instanceof ArrayList) {
                                        for (sys in taxonsys) {
                                            'gmd:classSys' {
                                                'gmd:CI_Citation' {
                                                    'gmd:title' {
                                                        'gco:CharacterString'(sys['title'])
                                                    }
                                                    if (sys['pubdate']) {
                                                        'gmd:date' {
                                                            'gmd:CI_Date' {
                                                                'gmd:date' {
                                                                    'gco:Date'(sys['pubdate'])
                                                                }
                                                                'gmd:dateType' {
                                                                    'gmd:CI_DateTypeCode'(codeList: CODE_LIST +
                                                                            '#CI_DateTypeCode', codeListValue: 'publication',
                                                                            'publication')
                                                                }
                                                            }
                                                        }
                                                    } else {
                                                        'gmd:date'('gco:nilReason', 'unknown')
                                                    }
                                                    'gmd:citedResponsibleParty' {
                                                        'gmd:CI_ResponsibleParty' {
                                                            'gmd:organisationName' {
                                                                'gco:CharacterString'(sys['origin'])
                                                            }
                                                            if (sys['ptcontac']) {
                                                                'gmd:contactInfo' {
                                                                    'gmd:CI_Contact' {
                                                                        'gmd:role' {
                                                                            'gmd:CI_RoleCode'(codeList: CODE_LIST +
                                                                                    '#CI_RoleCode',
                                                                                    codeListValue: 'originator', 'originator')
                                                                        }
                                                                    }
                                                                }
                                                            } else {
                                                                'gmd:contactInfo'('gco:nilReason': 'unknown')
                                                            }
                                                            'gmd:role' {
                                                                'gmd:CI_RoleCode'(codeList: CODE_LIST +
                                                                        '#CI_RoleCode', codeListValue: 'originator',
                                                                        'originator')
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    } else {
                                        'gmd:classSys' {

                                        }
                                    }
                                } else {
                                    'gmd:classSys'('gco:nilReason': 'unknown')
                                }

                                if (taxon['taxongen']) {
                                    'gmd:taxongen' {
                                        'gco:CharacterString'(WordUtils.wrap(taxon['taxongen'], 80))
                                    }
                                }

                                'gmd:idref'('gco:nilReason': 'unknown')

                                if (taxon['taxonpro']) {
                                    'gmd:taxonpro' {
                                        'gco:CharacterString'(WordUtils.wrap(taxon['taxonpro'], 80))
                                    }
                                } else {
                                    'gmd:taxonpro'('gco:nilReason': 'unknown')
                                }

                                taxonXml = insertTaxonomyRecursively(reader.depthFirst().find { it.name() == 'taxoncl' })

                                if (taxonXml.size() > 1) {
                                    taxonXml = Eval.me("{-> ${taxonXml} }")
                                    taxonXml.delegate = delegate
                                    taxonXml()
                                } else {
                                    'gmd:taxonCl'('gco:nilReason': 'unknown')
                                }
                            }
                        }
                    }
                    'gmd:language' {
                        'gco:CharacterString'('eng; USA')
                    }
                    'gmd:extent' {
                        'gmd:EX_Extent'(id: 'boundingExtent') {
                            'gmd:geographicElement' {
                                'gmd:EX_GeographicBoundingBox'(id: 'boundingGeographicBoundingBox') {
                                    if (idinfo['westbc']) {
                                        'gmd:westBoundLongitude' {
                                            'gco:Decimal'(idinfo['westbc'])
                                        }
                                    } else {
                                        'gmd:westBoundLongitude'('gco:nilReason': 'missing')
                                    }

                                    if (idinfo['eastbc']) {
                                        'gmd:eastBoundLongitude' {
                                            'gco:Decimal'(idinfo['eastbc'])
                                        }
                                    } else {
                                        'gmd:eastBoundLongitude'('gco:nilReason': 'missing')
                                    }

                                    if (idinfo['southbc']) {
                                        'gmd:southBoundLatitude' {
                                            'gco:Decimal'(idinfo['southbc'])
                                        }
                                    } else {
                                        'gmd:southBoundLatitude'('gco:nilReason': 'missing')
                                    }

                                    if (idinfo['northbc']) {
                                        'gmd:northBoundLatitude' {
                                            'gco:Decimal'(idinfo['northbc'])
                                        }
                                    } else {
                                        'gmd:northBoundLatitude'('gco:nilReason': 'missing')
                                    }
                                }
                            }
                            'gmd:temporalElement' {
                                'gmd:EX_TemporalExtent'(id: 'boundingTemporalExtent') {
                                    'gmd:extent' {
                                        'gml:TimePeriod'('gml:id': 'boundingTimePeriodExtent') {
                                            'gml:description'(idinfo['current'])
                                            'gml:beginPosition'(idinfo['begdate'])
                                            'gml:endPosition'(idinfo['enddate'])
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (eainfo) {
                'gmd:contentInfo' {
                    'gmd:MD_FeatureCatalogueDescription' {
                        'gmd:includedWithDataset' {
                            'gco:Boolean'('true')
                        }
                        'gmd:featureCatalogueCitation' {
                            'gmd:CI_Citation' {
                                'gmd:title' {
                                    'gco:CharacterString'('Feature Catalogue for ' + idinfo['title'])
                                }
                                'gmd:date'('gco:nilReason': 'unknown')
                                'gmd:citedResponsibleParty' {
                                    'gmd:CI_ResponsibleParty' {
                                        'gmd:individualName' {
                                            'gco:CharacterString'(metainfo['cntper'])
                                        }
                                        'gmd:organisationName' {
                                            'gco:CharacterString'(metainfo['cntorg'])
                                        }
                                        'gmd:contactInfo' {
                                            'gmd:CI_Contact' {
                                                'gmd:phone' {
                                                    'gmd:CI_Telephone' {
                                                        if (metainfo['cntvoice'] instanceof ArrayList && metainfo['cntvoice'].size() > 1) {
                                                            for (phone in metainfo['cntvoice']) {
                                                                'gmd:voice' {
                                                                    'gco:CharacterString'(phone)
                                                                }
                                                            }
                                                        } else {
                                                            'gmd:voice' {
                                                                'gco:CharacterString'(metainfo['cntvoice'])
                                                            }
                                                        }
                                                        'gmd:facsimile' {
                                                            'gco:CharacterString'(metainfo['cntfax'])
                                                        }
                                                    }
                                                }
                                                'gmd:address' {
                                                    'gmd:CI_Address' {
                                                        'gmd:deliveryPoint' {
                                                            'gco:CharacterString'(metainfo['address'])
                                                        }
                                                        'gmd:city' {
                                                            'gco:CharacterString'(metainfo['city'])
                                                        }
                                                        'gmd:administrativeArea' {
                                                            'gco:CharacterString'(metainfo['state'])
                                                        }
                                                        'gmd:postalCode' {
                                                            'gco:CharacterString'(metainfo['postal'])
                                                        }
                                                        'gmd:country' {
                                                            'gco:CharacterString'(metainfo['country'])
                                                        }
                                                        if (metainfo['cntemail'] instanceof ArrayList && metainfo['cntemail'].size() > 1) {
                                                            for (email in metainfo['cntemail']) {
                                                                'gmd:electronicMailAddress' {
                                                                    'gco:CharacterString'(removeAtSignFromEmailAddress(email))
                                                                }
                                                            }
                                                        } else {
                                                            'gmd:electronicMailAddress' {
                                                                'gco:CharacterString'(removeAtSignFromEmailAddress(metainfo['cntemail']))
                                                            }
                                                        }
                                                    }
                                                }
                                                'gmd:onlineResource' {
                                                    'gmd:CI_OnlineResource' {
                                                        'gmd:linkage' {
                                                            'gmd:URL'(file_ident.toString())
                                                        }
                                                        'gmd:protocol' {
                                                            'gco:CharacterString'('file')
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        'gmd:role' {
                                            'gmd:CI_RoleCode'(codeList: CODE_LIST + '#CI_RoleCode', codeListValue: 'pointOfContact', 'pointOfContact')
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                'gmd:contentInfo' {
                    'gmd:MD_FeatureCatalogueDescription' {
                        'gmd:includedWithDataset' {
                            'gco:Boolean'('false')
                        }
                        'gmd:featureCatalogueCitation'('gco:nilReason': 'inapplicable')
                    }
                }
            }
            'gmd:distributionInfo' {
                'gmd:MD_Distribution' {
                    def digform = getMetadataValue('digform', true)
                    if (digform instanceof ArrayList && digform.size() > 1) {
                        for (def format in digform) {
                            'gmd:distributionFormat' {
                                'gmd:MD_Format' {
                                    'gmd:name' {
                                        'gco:CharacterString'(format['formname'])
                                    }
                                    'gmd:version'('gco:nilReason': 'unknown')
                                    'gmd:fileDecompressionTechnique' {
                                        if (!format['filedec']) {
                                            'gco:CharacterString'('No compression applied')
                                        } else {
                                            'gco:CharacterString'(format['filedec'])
                                        }
                                    }
                                }
                            }
                        }
                    } else if (digform) {
                        'gmd:distributionFormat' {
                            'gmd:MD_Format' {
                                'gmd:name' {
                                    'gco:CharacterString'(digform['formname'])
                                }
                                'gmd:version'('gco:nilReason': 'unknown')
                                'gmd:fileDecompressionTechnique' {
                                    if (!digform['filedec']) {
                                        'gco:CharacterString'('No compression applied')
                                    } else {
                                        'gco:CharacterString'(digform['filedec'])
                                    }
                                }
                            }
                        }
                    }
                    if (distinfo) {
                        'gmd:distributor' {
                            'gmd:MD_Distributor' {
                                'gmd:distributorContact' {
                                    'gmd:CI_ResponsibleParty' {
                                        'gmd:individualName' {
                                            'gco:CharacterString'(distinfo['cntper'])
                                        }
                                        'gmd:organisationName' {
                                            'gco:CharacterString'(distinfo['cntorg'])
                                        }
                                        'gmd:contactInfo' {
                                            'gmd:CI_Contact' {
                                                'gmd:phone' {
                                                    'gmd:CI_Telephone' {
                                                        'gmd:voice' {
                                                            'gco:CharacterString'(distinfo['cntvoice'])
                                                        }
                                                    }
                                                }
                                                'gmd:address' {
                                                    'gmd:CI_Address' {
                                                        'gmd:deliveryPoint' {
                                                            'gco:CharacterString'(distinfo['address'])
                                                        }
                                                        'gmd:city' {
                                                            'gco:CharacterString'(distinfo['city'])
                                                        }
                                                        'gmd:administrativeArea' {
                                                            'gco:CharacterString'(distinfo['state'])
                                                        }
                                                        'gmd:postalCode' {
                                                            'gco:CharacterString'(distinfo['postal'])
                                                        }
                                                        'gmd:country' {
                                                            'gco:CharacterString'(distinfo['country'])
                                                        }
                                                        'gmd:electronicMailAddress' {
                                                            if (distinfo['cntemail'].size() > 1) {
                                                                for (def s in distinfo['cntemail']) {
                                                                    if (s.contains('codiac')) {
                                                                        'gco:CharacterString'(removeAtSignFromEmailAddress(s))
                                                                        break;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        'gmd:role' {
                                            'gmd:CI_RoleCode'(codeList: CODE_LIST + '#CI_RoleCode',
                                                    codeListValue: 'distributor', 'distributor')
                                        }
                                    }
                                }
                                'gmd:distributionOrderProcess' {
                                    'gmd:MD_StandardOrderProcess' {
                                        'gmd:fees' {
                                            'gco:CharacterString'('none')
                                        }
                                    }
                                }
                            }
                        }
                        'gmd:transferOptions' {
                            'gmd:MD_DigitalTransferOptions' {
                                'gmd:onLine' {
                                    'gmd:CI_OnlineResource' {
                                        'gmd:linkage' {
                                            if (distinfo['networkr'] instanceof ArrayList) {
                                                'gmd:URL'(distinfo['networkr'][0])
                                            } else {
                                                'gmd:URL'(distinfo['networkr'])
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        'gmd:distributor'('gco:nilReason': 'unknown')
                    }
                }
            }
            'gmd:dataQualityInfo' {
                'gmd:DQ_DataQuality' {
                    'gmd:scope'('gco:nilReason': 'unknown')
                    'gmd:report' {
                        'gmd:DQ_CompletenessCommission' {
                            'gmd:result'('gco:nilReason': 'unknown')
                        }
                    }
                    'gmd:report' {
                        'gmd:DQ_CompletenessOmission' {
                            'gmd:evaluationMethodDescription' {
                                'gco:CharacterString'('Unspecified')
                            }
                            'gmd:result'('gco:nilReason': 'unknown')
                        }
                    }
                    'gmd:report' {
                        'gmd:DQ_ConceptualConsistency' {
                            'gmd:measureDescription' {
                                if (dataqual) {
                                    'gco:CharacterString'(dataqual['logic'] ?: 'Unspecified')
                                } else {
                                    'gco:CharacterString'('Unspecified')
                                }
                            }
                            'gmd:result'('gco:nilReason': 'unknown')
                        }
                    }
                    'gmd:lineage' {
                        'gmd:LI_Lineage' {
                            if (dataqual) {
                                if (dataqual['procdesc'] instanceof ArrayList) {
                                    for (desc in dataqual['procdesc']) {
                                        'gmd:processStep' {
                                            'gmd:LI_ProcessStep' {
                                                'gmd:description' {
                                                    'gco:CharacterString'(WordUtils.wrap(desc, 80))
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    'gmd:processStep' {
                                        'gmd:LI_ProcessStep' {
                                            'gmd:description' {
                                                'gco:CharacterString'(WordUtils.wrap(dataqual['procdesc'], 80))
                                            }
                                        }
                                    }
                                }
                            } else {
                                'gmd:processStep'('gco:nilReason': 'missing')
                            }
                        }
                    }
                }
            }
            'gmd:metadataMaintenance' {
                'gmd:MD_MaintenanceInformation' {
                    'gmd:maintenanceAndUpdateFrequency' {
                        'gmd:MD_MaintenanceFrequencyCode'(codeList: CODE_LIST + '#MD_MaintenanceFrequencyCode',
                                codeListValue: 'asNeeded','asNeeded')
                    }
                    'gmd:maintenanceNote' {
                        'gco:CharacterString'('This metadata was automatically generated from an NCAR/CDS/EOL ' +
                                'proprietary tool designed to translate FGDC to ISO 19115-2')
                    }
                    'gmd:contact' {
                        'gmd:CI_ResponsibleParty' {
                            'gmd:individualName' {
                                'gco:CharacterString'(metainfo['cntper'])
                            }
                            'gmd:organisationName' {
                                'gco:CharacterString'(metainfo['cntorg'])
                            }
                            'gmd:contactInfo' {
                                'gmd:CI_Contact' {
                                    'gmd:phone' {
                                        'gmd:CI_Telephone' {
                                            'gmd:voice' {
                                                'gco:CharacterString'(metainfo['cntvoice'])
                                            }
                                        }
                                    }
                                    'gmd:address' {
                                        'gmd:CI_Address' {
                                            'gmd:deliveryPoint' {
                                                'gco:CharacterString'(metainfo['address'])
                                            }
                                            'gmd:city' {
                                                'gco:CharacterString'(metainfo['city'])
                                            }
                                            'gmd:administrativeArea' {
                                                'gco:CharacterString'(metainfo['state'])
                                            }
                                            'gmd:postalCode' {
                                                'gco:CharacterString'(metainfo['postal'])
                                            }
                                            'gmd:country' {
                                                'gco:CharacterString'(metainfo['country'])
                                            }
                                            if (metainfo['cntemail'] instanceof ArrayList) {
                                                for (def email in metainfo['cntemail']) {
                                                    if (!email.contains('codiac')) {
                                                        'gmd:electronicMailAddress' {
                                                            'gco:CharacterString'(removeAtSignFromEmailAddress(email))
                                                        }
                                                    }
                                                }
                                            } else {
                                                if (metainfo['cntemail']) {
                                                    'gmd:electronicMailAddress' {
                                                        'gco:CharacterString'(removeAtSignFromEmailAddress
                                                                (metainfo['cntemail']))
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            'gmd:role' {
                                'gmd:CI_RoleCode'(codeList: CODE_LIST + '#CI_RoleCode', codeListValue: 'custodian', 'custodian')
                            }
                        }
                    }
                }
            }
        }
    }

    def xml = XmlUtil.serialize(out)
    println 'Conversion complete...'

    if (file.lastIndexOf('/') > -1) {
        file = file.substring(file.lastIndexOf('/'))
    }

    if (file.lastIndexOf('.') > -1) {
        file = file.substring(0, file.lastIndexOf('.'))
    }

    if (output_dir) {
        def fileDir = new File(output_dir)
        if (!fileDir.exists()) {
            fileDir.mkdirs()
        }

        new File(fileDir, file + '_iso.xml').write(xml, 'UTF-8')
    } else {
        new File('./' + file + '_iso.xml').write(xml, 'UTF-8')
    }
}

processArgs(setupCli())

if (file instanceof ArrayList) {
    for (filename in file) {
        reader = new XmlSlurper().parse(filename)

        extractFgdc()
        createAsIso(filename.getName())
    }
} else {
    try {
        reader = new XmlSlurper().parse(new File(file))
    } catch (e) {
        println 'Error: ' + e.getMessage() + '\nExiting...'
        System.exit(1)
    }

    extractFgdc()
    createAsIso(file)
}