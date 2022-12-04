(defpackage :rb-vector
  (:use :cl)
  (:shadow :pop :count :equal :reduce :map :remove :append)
  (:export

   ;; Both a type and a creation function
   :rb-vector

   ;; Functions for operating on rb-vectors
   :lookup
   :insert
   :append
   :count
   :pop
   :slice
   :peek
   :equal
   :concat
   :remove

   ;; higher-order functions
   :map
   :reduce
   :filter

   ;; Utilities
   :to-list
   :from-sequence))
