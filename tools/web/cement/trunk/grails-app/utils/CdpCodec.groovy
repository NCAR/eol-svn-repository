
/*
 encode a string for the CDP - ugh :(
  7-bit clean printing ASCII only
  normal XML-entities (e.g. &amp;) are not allowed
*/

class CdpCodec {

  static encode = { s ->
   try {
    return s.
     replaceAll(/[\x09\x0A\x0D]/,' ').
     replaceAll(/[^\x20-\x7E]/,'?').
     replace("'" as char,' ' as char).
     replace('"' as char,' ' as char).
     //replaceAll(/&[A-Za-z]{1,9};/,'?').
     //replaceAll(/&#[0-9]{1,4};/,'?').
     //replaceAll(/&#[Xx][0-9A-Fa-f]{1,4};/,'?').
     replaceAll(/\s*&\s*/,' and ').
     replaceAll(/\s*<\s*=\s*/,' is less than or equal to ').
     replaceAll(/\s*>\s*=\s*/,' is greater than or equal to ').
     replaceAll(/\s*<\s*/,' is less than ').
     replaceAll(/\s*>\s*/,' is greater than ')
    } catch (Exception e) {
      return ''
      }
   }

}
