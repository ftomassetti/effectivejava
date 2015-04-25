(defproject effectivejava "0.1.3-SNAPSHOT"
  :description "A Java linter and a tool for running queries on your Java codebase"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.github.javaparser/javaparser-core "2.0.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [instaparse "1.3.6"]]
  :resource-paths ["test-resources"]
  :main app.core)