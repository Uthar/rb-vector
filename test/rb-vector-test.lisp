(in-package :rb-vector/test)

(in-suite :rb-vector)

(defun sequence->rb-vector (sequence)
  (cl:reduce #'rb-vector:append sequence :initial-value (rb-vector)))

(defun range (n)
  (loop for x below n collect x))

(test empty-rb-vector
  (let* ((rb-vector (rb-vector)))
    (is (= (count rb-vector) 0))))

(test 1-rb-vector
  (let* ((rb-vector (rb-vector "foo")))
    (is (= (count rb-vector) 1))
    (is (equal (lookup rb-vector 0) "foo"))))

(test 100k-rb-vector
  (let* ((rb-vector (sequence->rb-vector (range 100000))))
    (is (= (count rb-vector) 100000))
    (is (loop for n below 100000 always (= (lookup rb-vector n) n)))))

(test immutable
  (let* ((tup1 (rb-vector "foo" "bar"))
         (tup2 (append tup1 "baz")))
    (insert tup2 0 :x)
    (is (equal (lookup tup1 0) "foo"))
    (is (equal (lookup tup2 0) "foo"))
    (insert tup2 1 :y)
    (is (equal (lookup tup1 1) "bar"))
    (is (equal (lookup tup2 1) "bar"))
    (is (equal (lookup tup2 2) "baz"))))

(test tail-copied
  (let* ((tup1 (sequence->rb-vector (range 32)))
         (tup2 (append tup1 :foo)))
    (is (= (count tup1) 32))
    (is (= (count tup2) 33))
    (is (= (length (rb-vector::rb-vector-tail tup1)) 32))
    (is (not (eq (rb-vector::rb-vector-tail tup2) (rb-vector::rb-vector-tail tup1))))
    (is (equalp (rb-vector::rb-vector-tail tup1)
                (rb-vector::node-array (aref (rb-vector::node-array (rb-vector::rb-vector-root tup2)) 0))))
    (is (loop with tail = (rb-vector::rb-vector-tail tup1)
              for n below 32
              always (= (aref tail n) n)))
    (is (eq (aref (rb-vector::rb-vector-tail tup2) 0) :foo))))

(test share-root
  (let* ((tup1 (sequence->rb-vector (range 1056)))
         (tup2 (append tup1 :foo)))
    (is (= (count tup1) 1056))
    (is (= (count tup2) 1057))
    (is (= (length (rb-vector::rb-vector-tail tup1)) 32))
    (is (not (eq (rb-vector::rb-vector-tail tup2) (rb-vector::rb-vector-tail tup1))))
    (is (eq (rb-vector::rb-vector-root tup1) (aref (rb-vector::node-array (rb-vector::rb-vector-root tup2)) 0)))
    (is (eq (aref (rb-vector::rb-vector-tail tup2) 0) :foo))))


(test 1k-insert
  (let* ((rb-vector (sequence->rb-vector (range 1057)))
         (vals (make-array 1057
                           :initial-contents
                           (loop for n below 1057 collect (funcall (gen-string))))))
    (is (= (count rb-vector) 1057))
    (is (loop for n below 1057
              for tup = (insert rb-vector n (aref vals n)) then (insert tup n (aref vals n))
              always (loop for x below n always (equal (lookup tup x) (aref vals x)))
              always (loop for x from (1+ n) below 1057 always (= (lookup tup x) x))))))

(test pop-test
  (is (= 3 (count (pop (rb-vector 1 2 3 4)))))
  (is (= 1056 (count (pop (sequence->rb-vector (range 1057))))))
  (is (= 32 (count (pop (sequence->rb-vector (range 33))))))

  (is (eq :foo (peek (append (pop (pop (rb-vector 1 2 3 4))) :foo))))
  (is (eq :foo (peek (append (pop (pop (sequence->rb-vector (range 1057)))) :foo))))
  (is (eq :foo (peek (append (pop (pop (sequence->rb-vector (range 33)))) :foo)))))


(test slice-test
  (is (= 3 (count (slice (rb-vector 1 2 3 4) 1))))
  (is (= 1056 (count (slice (sequence->rb-vector (range 1057)) 1))))
  (is (= 32 (count (slice (sequence->rb-vector (range 33)) 1))))

  (is (= 2 (count (slice (slice (rb-vector 1 2 3 4) 1) 1))))
  (is (= 1055 (count (slice (slice (sequence->rb-vector (range 1057)) 1) 1))))
  (is (= 31 (count (slice (slice (sequence->rb-vector (range 33)) 1) 1)))))

(test equal-test
  (is (rb-vector:equal (rb-vector 1 2 3) (rb-vector 1 2 3)))
  (is (rb-vector:equal (slice (rb-vector -1 0 1 2 3 4 5) 2 5) (rb-vector 1 2 3)))
  (is (rb-vector:equal (rb-vector 1 2 3) (slice (rb-vector -1 0 1 2 3 4 5) 2 5) ))
  (is (rb-vector:equal (slice (rb-vector -1 0 1 2 3 4 5) 2 5) (slice (rb-vector -1 0 1 2 3 4 5) 2 5) ))
  (is (rb-vector:equal (slice (slice (rb-vector -1 0 1 2 3 4 5) 2 5) 1) (rb-vector 2 3)))
  (is (rb-vector:equal (rb-vector 2 3) (slice (slice (rb-vector -1 0 1 2 3 4 5) 2 5) 1) ))
  (is (rb-vector:equal (slice (slice (rb-vector -1 0 1 2 3 4 5) 2 5) 1)
                   (slice (slice (rb-vector -1 0 1 2 3 4 5) 2 5) 1)))

  (is (rb-vector:equal (rb-vector (rb-vector (rb-vector 1))) (rb-vector (rb-vector (rb-vector 1)))))
  (is (rb-vector:equal (slice (rb-vector :foo (rb-vector (rb-vector 1))) 1) (rb-vector (rb-vector (rb-vector 1))))))

(test concat-test
  (is (rb-vector:equal (rb-vector 1 2 3 4) (rb-vector:concat (rb-vector 1 2) (rb-vector 3 4))))
  (is (rb-vector:equal (rb-vector:concat (rb-vector 1 2) (rb-vector 3 4)) (rb-vector 1 2 3 4)))
  (is (rb-vector:equal (rb-vector:concat (rb-vector 1 2) (rb-vector 3 4)) (slice (rb-vector -1 0 1 2 3 4 5 6) 2 6)))
  (is (rb-vector:equal (rb-vector:concat (rb-vector 1 2) (slice (rb-vector 1 2 3 4 5 6) 2 6)) (rb-vector 1 2 3 4 5 6))))

(test remove-test
  (is (rb-vector:equal (rb-vector 1 2 3 4) (rb-vector:remove (rb-vector 0 1 2 3 4) 0)))
  (is (rb-vector:equal (rb-vector 0 2 3 4) (rb-vector:remove (rb-vector 0 1 2 3 4) 1)))
  (is (rb-vector:equal (rb-vector 0 1 3 4) (rb-vector:remove (rb-vector 0 1 2 3 4) 2)))
  (is (rb-vector:equal (rb-vector 0 1 2 4) (rb-vector:remove (rb-vector 0 1 2 3 4) 3)))
  (is (rb-vector:equal (rb-vector 0 1 2 3) (rb-vector:remove (rb-vector 0 1 2 3 4) 4))))

(test cat-test                                                           
  (is (rb-vector:equal (rb-vector 1 10) (insert (remove (rb-vector 1 2 3) 1) 10)))   
  (is (rb-vector:equal (rb-vector 1 3 10) (append (remove (rb-vector 1 2 3) 1) 10))))
