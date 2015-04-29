effectivejava
=============

[![Build Status](https://travis-ci.org/ftomassetti/effectivejava.svg?branch=master)](https://travis-ci.org/ftomassetti/effectivejava)

Effective java is a tool to examine your Java codebase. You can use it in three different ways:
* as a Java linter. Just tells it which directory you want to examine, it will spit out a set of warnings and suggestions to improve your code
* run queries from the command line. It could tell you which type of Singleton you are using in your codebase or how many constructors have 10 or more parameters. This modality can be easily integrated with other tools
* run queries interactively. It permits to poke your codebase, parsing it once and running different queries to find out interesting facts about it.

The project is named effectivajava because many queries/checks derive from reading the book Effective Java. Others will be implemented as well (feel free to suggest your favorite ones!).

While reading that book I thought that yes, many principles are well known, but they are rarely applied to a large codebase. I thought that applying them in practice is much harder than it seems, and a tool like this one could help in improving constantly a codebase.

Which is the easiest way to install it?
=======================================

Download the standalone jar from the releases directory. 
No deps needed, everything is packed inside the jar.
Feel free to rename it (effectivejava-0.1.0-SNAPSHOT-standalone.jar is a mouthful...)

Linting mode: how to use it
===========================

Just run:

```bash
# this generate a jar file
lein jar
# note that 0.1.3 is the current version it could change in the future
java -jar effectivejava-0.1.3-SNAPSHOT-standalone.jar -l -d "<myJavaProjectDir>"
```

You can expect a set of lines like this one:
```
org.springframework.jdbc.core.SqlInOutParameter : This class has too many constructors (7). Consider using static factory methods or the Builder pattern
```

If you run this command from the root of your codebase you can avoid the -d option.

CLI mode: how to use it
=======================

Now, suppose you want to know which classes has 5 or more constructor; you can run this command:
```bash
java -jar effectivejava-0.1.1-SNAPSHOT-standalone.jar -q mc -d "<myJavaProjectDir>" -t 5
```
You can expect a similar output:
```
Considering 109 Java files
japa.parser.ast.expr.ArrayCreationExpr  :  5
japa.parser.ast.body.MethodDeclaration  :  5
japa.parser.ast.body.BaseParameter  :  5
japa.parser.ast.body.FieldDeclaration  :  5
```

Interactive mode: how to use it (Work in progress!)
===================================================

You can launch interactive mode with the -i option.

```bash
java -jar effectivejava-0.1.1-SNAPSHOT-standalone.jar -i
```

A typical interaction could be this one:

```
> load "."
Loading .
Java files loaded: 440
> mc th 5
Command not implemented:  :MC
```

As you can read from the last line, while the main logic for the interactive mode is there we still miss a few bits :)
It will be corrected soon.

What queries can you run
========================
I am just getting started so I implemented only a few queries for now:

* _mc=many constructors_: find the classes which contain the same number of constructors as the given threshold, or more
* _mcp=many constructor parameters_: find the constructors which contain the same number of parameters as the given threshold, or more
* _st=singleton type_: find if a type implements the singleton pattern and distinguish between the three types (public field, static factory, singleton enum)
* _u=utils classes_: find classes having only static methods and verify they have exactly one private constructor taking no parameters

Dev info
========
The project is written in Clojure using a java library called [JavaParser](https://github.com/javaparser/javaparser).

You will need also [Leiningen](http://leiningen.org/), the build tool for Clojure. It should download also Clojure for you.

Dev guidelines
==============

Use [kibit](https://github.com/jonase/kibit) and [eastwood](https://github.com/jonase/eastwood) to verify code quality. You may want to add the to the plugins section of your `~/.lein/profiles.clj`.

When running eastwood exclude the check for unlimited use of namespaces:

```
lein eastwood "{:exclude-linters [:unlimited-use]}"
```

What is the link with the book?
===============================
I am reading this book and many advices seem sort of obvious in theory but I guess there are some violations lurking in the large codebase I am working with. I was curious to assess how many violations there were and I needed a way to find them out automatically.
And I wanted to learn Clojure.
And I had a free sunday.
So...

What else
=========
Hope you enjoy this small project of mine. Feel free to open issues and ask questions!

Contributors
============

Thanks to [David Ortiz](https://github.com/davidor) for fixing bugs and setting up Travis.
