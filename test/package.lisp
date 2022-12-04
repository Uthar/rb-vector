(defpackage :rb-vector/test
  (:use :cl :5am :rb-vector)
  (:shadowing-import-from :cl :equal :reduce :map)
  (:shadowing-import-from :rb-vector :pop :count :append :remove))

(in-package :rb-vector/test)

(def-suite :rb-vector)
