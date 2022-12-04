(defsystem :rb-vector
  :description "Immutable vector data structure"
  :version "0.1.0"
  :license "GPLv3"
  :pathname "src"
  :components ((:file "package")
               (:file "rb-vector"))
  :in-order-to ((test-op (test-op :rb-vector/test))))

(defsystem :rb-vector/test
  :depends-on ("rb-vector" "fiveam")
  :pathname "test"
  :components ((:file "package")
               (:file "rb-vector-test"))
  :perform (test-op (o c) (symbol-call :5am :run! :rb-vector)))

(defsystem :rb-vector/bench
  :depends-on ("rb-vector" "trivial-garbage")
  :pathname "bench"
  :components ((:file "bench")))

(defsystem :rb-vector/reader
  :depends-on ("rb-vector")
  :pathname "src"
  :components ((:file "reader")))
