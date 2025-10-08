class CdpTagLib {
  static namespace = "cdp"

  def dataType = { attrs, body ->
    def cdpDataType = null
    switch (attrs.type) {
    case "grid":
      cdpDataType = "Grid"
      break
    case "point":
      cdpDataType = "Point"
      break
    case "raster":
      cdpDataType = "Image"
      break
    case "multiple":
      // no THREDDS value
      // FALL-THRU
    case "unknown":
      // no THREDDS value
      // FALL-THRU
    default:
      // no THREDDS value
      break
    }
    if (cdpDataType)
      out << '<dataType type="'
      out << cdpDataType
      out << '" />'
  }

}
