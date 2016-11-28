java-adt=../dist/build/java-adt/java-adt

.PRECIOUS : out/*.java out/%.java out

all : out out/Exp.class out/CMM.class out/List.class Stm.class out/CustomVisitorsStraightLine.class out/MJAbsSyn.class

# create default visitor
out/List.java : List.hs
	$(java-adt) -d -o $@ $<

# create public files
Stm.java : straightLine.hs
	$(java-adt) -p $<

out/CustomVisitorsStraightLine.java : customVisitorsStraightLine.hs
	$(java-adt) -o $@ $<

# create default visitor
out/MJAbsSyn.java : MJAbsSyn.hs
	$(java-adt) -d -o $@ $<

# don't create default visitor
out/%.java : %.hs
	$(java-adt) -o $@ $<

%.class : %.java
	javac $<

out :
	-mkdir $@

# EOF
