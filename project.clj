(defproject effectivejava "0.1.2-SNAPSHOT"
  :description "A Java linter and a tool for running queries on your Java codebase"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.google.code.javaparser/javaparser "1.0.11"]
                 [org.clojure/tools.cli "0.3.1"]
                 [instaparse "1.3.3"]]
  :resource-paths ["test-resources"]
  :main app.core)