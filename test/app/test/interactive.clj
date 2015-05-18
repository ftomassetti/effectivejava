(ns app.test.interactive
  (:use [app.interactive]
        [app.javaparser.navigation]
        [app.operations]
        [app.itemsOnLifecycle]
        [conjure.core]
        [clojure.test]))

(def javaparser-cus-path
  "test-resources/sample-codebases/javaparser/")

(defn- command-sequence->input-str [command-sequence]
  (clojure.string/join "\r" command-sequence))

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
        input-string (command-sequence->input-str command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush]
               (interactive [])
               (verify-call-times-for println 2)
               (verify-first-call-args-for println help-message)))))

(deftest list-shows-error-if-no-classes-loaded
  (let [command-sequence ["list" "quit"]
        input-string (command-sequence->input-str command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush]
               (interactive [])
               (verify-call-times-for println 2)
               (verify-first-call-args-for println
                                           no-classes-loaded-error)))))

(deftest can-execute-mc-operation
  ;; In this test we want to check whether the printOperation function of the
  ;; app.operations namespace is called with the correct parameters.
  ;; For this reason, we can use any compilation units and any threshold.
  (let [javaparser-cus {:cus (take 2 (cus javaparser-cus-path))}
        mc-op-threshold 3
        command-sequence [(str "mc th " mc-op-threshold) "quit"]
        input-string (command-sequence->input-str command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush printOperation]
               (interactive javaparser-cus)
               (verify-call-times-for printOperation 1)
               (verify-first-call-args-for
                printOperation
                classesWithManyConstructorsOp
                (:cus javaparser-cus)
                mc-op-threshold)))))
