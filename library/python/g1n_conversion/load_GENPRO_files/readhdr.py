#!/usr/bin/env python

###
# This is a python script that reads GENPRO-I headers
# will potentially be used as a module?
###

import sys, re

class readhdr():
    def __init__(self):

        header = self.loadHdr()
        self.parseHdr(header)

    def loadHdr(self):

        headerName = "/net/www/docs/raf/Headers/header.3-736" #ATLAS header file
        openHeader = open(headerName)
        header = openHeader.read()
        print header
        return header

    def parseHdr(self, header):

        firstLine = str(header.split('\n', 1)[0])
        projmeta = re.findall("\S+", firstLine)
        projNum = projmeta[0]
        projName = projmeta[1]

        temp1 = header.split("FOLLOW", 1)[1]
        varList = temp1.split("\n\n", 1)[0]
        varList = varList.split('\n')
        varName = ["order", "rate", "plot title", "print lab", "units", "ad scale", "p scale"]

        for line in varList[1:]:

            vars = [None] * 7

            temp = re.split("\s{2,}", line)
            temp.pop(0) #first item is blank

            print temp

            vars[0] = temp[0].replace(")", "")
            vars[1] = temp[1]

            if len(temp) > 7: # if parsed correctly, the list should only hold 7 items
                if re.findall("^(.+)$", temp[3]): # if the plot title was separated
                    vars[2] = temp[2].join(temp[3])
                elif not findall("^(.+)$", temp[3]):
                    vars[2] = temp[2]


                #vars[5] = re.findall("=\s(.+)\s-", line) # AD scale
                #vars[6] = re.findall("-\s+(.+)$", line) # P scale
                #print temp

            else:

                for i in range(2,6):
                    vars[i] = temp[i]

            print vars


readhdr()


