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
               (verify-first-call-args-for println exit-message)))))

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

;; All the operations can be tested in the same way.
(defn- test-operation [op-command operation & [threshold]]
  (let [javaparser-cus {:cus (take 2 (cus javaparser-cus-path))}
        first-command (if threshold
                        (str op-command " th " threshold)
                        (str op-command))
        command-sequence [first-command "quit"]
        input-string (command-sequence->input-str command-sequence)]
    (with-in-str
      input-string
      (mocking [println print flush printOperation]
               (interactive javaparser-cus)
               (verify-call-times-for printOperation 1)
               (verify-first-call-args-for
                printOperation operation (:cus javaparser-cus) threshold)))))

(deftest can-execute-mc-operation
  (test-operation "mc" classesWithManyConstructorsOp 2))

(deftest can-execute-mcp-operation
  (test-operation "mcp" constructorsWithManyParametersOp 3))

(deftest can-execute-f-operation
  (test-operation "f" finalizersOp))

(deftest can-execute-st-operation
  (test-operation "st" classesAndSingletonTypeOp))
