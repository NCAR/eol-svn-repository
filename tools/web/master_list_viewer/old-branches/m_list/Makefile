#
# simple Makefile for "static" file editors
#

JAVA_HOME=/usr/local/java/jdk
ANT_HOME=/usr/local/java/ant
ANT=${ANT_HOME}/bin/ant
PATH:=${JAVA_HOME}/bin:${ANT_HOME}/bin:${PATH}

all:	install

install:	install-static

install-static:
	${ANT} deploy-static

