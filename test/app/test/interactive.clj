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


;; The next four tests check that the operations that can
;; be used from the interactive mode (mc, mcp, etc.) work as expected.
;; We are interested in checking whether the printOperation function of the
;; app.operations namespace is called with the correct parameters.
;; For this reason, we can use any compilation units and any threshold (for
;; the operations that require one).

(deftest can-execute-mc-operation
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

(deftest can-execute-mcp-operation
  (let [javaparser-cus {:cus (take 2 (cus javaparser-cus-path))}
        mcp-op-threshold 3
        command-sequence [(str "mcp th " mcp-op-threshold) "quit"]
        input-string (command-sequence->input-str command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush printOperation]
               (interactive javaparser-cus)
               (verify-call-times-for printOperation 1)
               (verify-first-call-args-for
                 printOperation
                 constructorsWithManyParametersOp
                 (:cus javaparser-cus)
                 mcp-op-threshold)))))

(deftest can-execute-st-operation
  (let [javaparser-cus {:cus (take 2 (cus javaparser-cus-path))}
        command-sequence ["st" "quit"]
        input-string (command-sequence->input-str command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush printOperation]
               (interactive javaparser-cus)
               (verify-call-times-for printOperation 1)
               (verify-first-call-args-for
                 printOperation
                 classesAndSingletonTypeOp
                 (:cus javaparser-cus)
                 nil)))))

(deftest can-execute-f-operation
  (let [javaparser-cus {:cus (take 2 (cus javaparser-cus-path))}
        command-sequence ["f" "quit"]
        input-string (command-sequence->input-str command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush printOperation]
               (interactive javaparser-cus)
               (verify-call-times-for printOperation 1)
               (verify-first-call-args-for
                 printOperation
                 finalizersOp
                 (:cus javaparser-cus)
                 nil)))))
