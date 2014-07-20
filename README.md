effectivejava
=============
Run queries on your Java code to check if it meets the criteria suggested by the book Effective Java. And some others, soon.

How to use it
=============
Download the standalone jar from the releases directory. 
No deps needed, everything is packed inside the jar.
Feel free to rename it (effectivejava-0.1.0-SNAPSHOT-standalone.jar is a mouthful...)

Now, suppose you want to know which classes has 5 or more constructor; you can run this command:
```bash
java -jar effectivejava-0.1.0-SNAPSHOT-standalone.jar -q mc -d "<myJavaProjectDir>" -t 5
```
You can expect a similar output:
```
Considering 109 Java files
japa.parser.ast.expr.ArrayCreationExpr  :  5
japa.parser.ast.body.MethodDeclaration  :  5
japa.parser.ast.body.BaseParameter  :  5
japa.parser.ast.body.FieldDeclaration  :  5
```

What queries can I run
======================
I am just getting started so I implemented only a couple of queries for now:

* mc=many constructors : find the classes which contain the same number of constructors as the given threshold, or more
* mcp=many constructor parameters : find the constructors which contain the same number of parameters as the given threshold, or more

Dev info
========
The project is written in Clojure using a java library called [JavaParser](https://github.com/matozoid/javaparser). Stricly speaking it is using [my fork]((https://github.com/ftomassetti/javaparser)) at this moment, but my chages will be soon getting merged in the main branch.

When my changes will find their way in the main branch I will update project.clj and javaparser will be download from Maven automatically. For the time being, if you want to play with the code you can clone my project and compile it to your local maven repository. Look at the other MD file in this dir, there there are the commands to run to do it.

You will need also [Leiningen](http://leiningen.org/), the build tool for Clojure. It should download also Clojure for you.

What is the link with the book?
===============================
I am reading this book and many advices seem sort of obvious in theory but I guess there are some violations lurking in the large codebase I am working with. I was curious to assess how many violations there were and I needed a way to find them out automatically.
And I wanted to learn Clojure.
And I had a free sunday.
So...


What else
=========
Hope you enjoy this small project of mine. Feel free to open issues and ask questions!
