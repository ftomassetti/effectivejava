(defproject effectivejava "0.2.0-SNAPSHOT"
  :description "A Java linter and a tool for running queries on your Java codebase"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.github.javaparser/javaparser-core "2.0.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [instaparse "1.3.6"]
                 [org.javassist/javassist "3.19.0-GA"]
                 [org.clojars.runa/conjure "2.1.3"]]
  :resource-paths ["test-resources"]
  :plugins [[lein-cljfmt "0.1.10"] [lein-ancient "0.6.7"] [lein-kibit "0.1.2"] 
            [jonase/eastwood "0.2.1"] [lein-cloverage "1.0.3"]]
  :license {:name "Apache License - v 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"
            :distribution :repo}
  :main app.core)