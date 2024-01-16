(ns bbg.util)

;; severities
;; - error
;; - warn
;; - info (alias log)
;; - debug


(defmacro error [& args]
  `(.error js/console ~@args))

(defmacro warn [& args]
  `(.warn js/console ~@args))

(defmacro log [& args]
  (when-not (= :release (:shadow.build/mode &env))
    `(.log js/console ~@args)))
