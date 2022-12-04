(require :asdf)
(pushnew (truename "../") asdf:*central-registry*)
(asdf:load-system :rb-vector)

(setf vec (rb-vector:rb-vector 1 2 "foo" 3.14))

(rb-vector:append vec "bar")

(rb-vector:pop vec)

(rb-vector:insert vec 0 "baz")

(rb-vector:lookup vec 3)

(rb-vector:peek vec)

(rb-vector:equal vec vec)

(rb-vector:slice vec 1 3)

(rb-vector:count vec)

(rb-vector:concat vec (rb-vector:rb-vector 42 43 44))

;; Optional `[` reader macro
(asdf:load-system :rb-vector/reader)
(rb-vector:equal [1 2 (+ 1 2)] (rb-vector:rb-vector 1 2 3))
