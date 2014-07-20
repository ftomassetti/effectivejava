(defproject effectivejava "0.1.1-SNAPSHOT"
  :description "A tool for running queries on your Java codebase"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.google.code.javaparser/javaparser "1.0.12-SNAPSHOT"]
                 [org.clojure/tools.cli "0.3.1"]
                 [instaparse "1.3.3"]]
  :main app.core)