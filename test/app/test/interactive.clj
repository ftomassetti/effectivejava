(ns app.test.interactive
  (:use [app.interactive]
        [conjure.core]
        [clojure.test]))

(deftest can-quit
  (let [input-command "quit\r"]
    (with-in-str
      input-command
      (mocking [println print flush]
               (interactive [])
               (verify-call-times-for println 1)
               (verify-first-call-args-for println "Exit...")))))

(deftest shows-help
  (let [command-sequence ["help" "quit"]
        input-string (clojure.string/join "\r" command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush]
               (interactive [])
               (verify-call-times-for println 2)
               (verify-first-call-args-for println help-message)))))

(deftest list-shows-error-if-no-classes-loaded
  (let [command-sequence ["list" "quit"]
        input-string (clojure.string/join "\r" command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush]
               (interactive [])
               (verify-call-times-for println 2)
               (verify-first-call-args-for println
                                           no-classes-loaded-error)))))
