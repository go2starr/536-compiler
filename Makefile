###
# This Makefile can be used to make a parser for the Little language
# (parser.class) and to make a program (P3.class) that tests the parser and
# the unparse methods in ast.java.
#
# make clean removes all generated files.
#
###

JC = javac

P6.class: P6.java parser.class Yylex.class ASTnode.class Codegen.class
	$(JC) -g P6.java

parser.class: parser.java ASTnode.class Yylex.class Errors.class
	$(JC) parser.java

parser.java: Little.cup
	java java_cup.Main < Little.cup

Yylex.class: Little.jlex.java sym.class Errors.class
	$(JC) Little.jlex.java

ASTnode.class: ast.java sym.class
	$(JC) -g ast.java

Little.jlex.java: Little.jlex sym.class
	java JLex.Main Little.jlex

sym.class: sym.java
	$(JC) -g sym.java

sym.java: Little.cup
	java java_cup.Main < Little.cup

Errors.class: Errors.java
	$(JC) Errors.java

Codegen.class:	Codegen.java
	$(JC) Codegen.java

###
# clean
###
clean:
	rm -f *~ *.class parser.java Little.jlex.java sym.java a.out *.tmp
