
JFLAGS = -g
.SUFFIXES: .java .class
.java.class:
	javac $(JFLAGS) $*.java

CLASSES = \
	IOUtils.java

default: classes

classes: $(CLASSES:.java=.class)

clean:
	rm *.class

