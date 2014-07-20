Build local version of javaparser:

mvn compile
mvn package
mvn install:install-file -Dfile=target/javaparser-1.0.12-SNAPSHOT.jar -DgroupId=com.google.code.javaparser -DartifactId=javaparser -Dversion=1.0.12-SNAPSHOT -Dpackaging=jar

