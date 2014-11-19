
JFLAGS = -g
.SUFFIXES: .java .class
.java.class:
	javac $(JFLAGS) $*.java

CLASSES = \
	fry/IOUtils.java \
	fry/FRYListFactory.java

default: classes

classes: $(CLASSES:.java=.class)

clean:
	rm fry/*.class

