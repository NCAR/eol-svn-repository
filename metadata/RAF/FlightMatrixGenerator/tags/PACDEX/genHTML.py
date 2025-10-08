# genHTML is a Python module to wrap webpage elements in html commands
# and return a string containing the wrapped text that can be written
# to an HTML file
#
import string

class RAFhtml:
    """generate various RAF html elements such as title and banner

    Most of these methods require the following input.
    project: the project name, i.e. PASE
    proj_num: a combo of the year and project number, i.e. 2007-114
    platform: the complete platform description string, i.e.
    "NSF/NCAR C-130Q Hercules (N130AR)"
    plat: a short version of the platform suitable for window titles, etc
    i.e. "C-130Q Hercules"
    
    """

    def row(self,colContent,rowtype, \
	    bgcolor="omitted",align="omitted",span="omitted",valign="omitted"):
    	""" 

	input: - a list containing a series of strings, one to be printed to
		each column in the row
		- the type of row (either a table header row 'th' or a
		  regular table row 'td'
	"""

	if align is not "omitted":
	    aln = ' align="'+align+'"'
	else:
	    aln = ''

	if valign is not "omitted":
	    valn = ' valign="'+valign+'"'
	else:
	    valn = ''


	if bgcolor is not "omitted":
	    bgc = ' bgcolor="'+bgcolor+'"'
	    # For now, just assume that if we set a bg color, we need to
	    # change the text font to white.  We can always make this more
	    # flexible later.
	    colContent = ['<font color="#ffffff">'+col+'</font>' \
		    for col in colContent]
	else:
	    bgc = ''

	endtag = '</'+rowtype+'>'
        if span is not "omitted":
            index=0
            while index < len(colContent):
	        tag = '<'+rowtype+aln+valn
	        colContent[index] = tag+ ' colspan = '+str(span[index])+'>'+ \
		    colContent[index]+endtag+'\n'
	        index=index+1
	else:
	    tag = '<'+rowtype+aln+valn+'>'
	    colContent = [tag + col + endtag + '\n' for col in colContent]
	rowstr = string.join(colContent,'')

	elements = ['<tr'+bgc+'>\n',rowstr,'\n</tr>\n']
	rowstr = string.join(elements,'')
	return rowstr

    def header_row(self,colContent,span="omitted",bgcolor="omitted"):
	#if bgcolor is not "omitted":
	rowstr = self.row(colContent,"th",bgcolor=bgcolor,span=span)
	#else:
	#    rowstr = self.row(colContent,"th")

	return rowstr

    def normal_row(self,colContent,align="omitted",valign="omitted"):
	#if align is not "omitted":
	rowstr = self.row(colContent,"td",align=align,valign=valign)
	#else:
	#    rowstr = self.row(colContent,"td")
	return rowstr

    def href(self,ref,string):
	"enclose a string in an 'a href' tag with a reference to ref"
	hstr = '<a href="'+ref+'">'+string+'</a>'

	return hstr

    def b(self,string):
	"enclose a string in a bold tag"
	hstr = '<b>'+string+'</b>'

	return hstr

    def h2center(self,string):
	"enclose a string in an 'h2 center' tag"
	str = '<center><h2>\n<p>'+string+'</p>\n</h2></center>\n'

	return str

    def image(self,image,alt,alignment):
	"""given an image with the path (absolute or relative) to it, generate
	an img tag
	
	alt: alternate text if image doesn't show
	alignment: "left" or "right"
	"""

	string = '<img alt="'+alt+'" align="'+alignment+ \
		'" border="0" src="'+ image + '">'

	return string

    def title(self,project,proj_num,plat,desc):
	title=["<title>",project,desc,"(Project"]
	title.append(proj_num+"), "+plat+"</title>")
	title = string.join(title)
	return title

    def banner(self,Limage,project,proj_num,longname,PI,platform,Rimage):
	"""assemble the project RAF web page banner
	
	i.e. pages under (and including) docsum.html. The banner consists
	of a project image on the left, a title in the middle, and the RAF
	meatball on the right.
	"""

	left = self.image(Limage,project + " Logo","left")
	right = self.image(Rimage,project + " Logo","right")
	right = self.href("/raf/",right)
	line1 = string.join(["Project #",proj_num,project])
	line2 = longname
	line3 = string.join(["Principal Investigator(s):",PI,"et al."])
	center = string.join([line1,line2,line3,platform],"</p>\n<p>")
	center = self.h2center(center)
	banner = self.normal_row([left,center,right])
	banner = string.join(['<table width="100%">\n',banner,'</table>'])
	banner = banner + "\n<hr noshade>\n"

	return banner
