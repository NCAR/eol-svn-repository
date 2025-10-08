
/*
 encode a URL for the CDP - ugh :(
*/

class CdpURLCodec {

  static encode = { s ->
   try {
    return s.
     replaceAll(/[\x09\x0A\x0D]/,' ').
     replaceAll(/[^\x20-\x7E]/,'?').
     replace("'" as char,' ' as char).
     replace('"' as char,' ' as char).
     replaceAll(/\s*&\s*/,'&amp;').
     replaceAll(/\s*<\s*=\s*/,' is less than or equal to ').
     replaceAll(/\s*>\s*=\s*/,' is greater than or equal to ').
     replaceAll(/\s*<\s*/,' is less than ').
     replaceAll(/\s*>\s*/,' is greater than ')
    } catch (Exception e) {
      return ''
      }
   }

}
