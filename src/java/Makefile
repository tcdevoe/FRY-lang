
JFLAGS = -g
.SUFFIXES: .java .class
.java.class:
	javac $(JFLAGS) $*.java

CLASSES = \
	fry/fry/FRYLayout.java \
	fry/fry/FRYTable.java \
	fry/fry/IOUtils.java \
	fry/fry/FRYListFactory.java 

default: classes

classes: $(CLASSES:.java=.class)

fry/fry/FRYTable.class : fry/fry/FRYLayout.java
fry/fry/IOUtils.class : fry/fry/FRYTable.java

clean:
	rm fry/fry/*.class

