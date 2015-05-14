(ns app.test.interactive
  (:use [app.interactive]
        [conjure.core]
        [clojure.test]))

(deftest can-quit
  (let [input-command "quit\r"]
    (with-in-str
      input-command
      (interactive []))))
