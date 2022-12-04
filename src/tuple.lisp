(in-package :rb-vector)

(declaim (optimize speed))

;;; API

(defgeneric rb-vector (&rest elems)
  (:documentation
   "Create a rb-vector containing elems"))

(defgeneric lookup (rb-vector index)
  (:documentation
   "Return the element at index"))

(defgeneric insert (rb-vector index val)
  (:documentation
   "Return a new rb-vector that has val at index"))

(defgeneric remove (rb-vector index)
  (:documentation
   "Return a new rb-vector with the value at index removed and the other
values shifted to fill the missing spot."))

(defgeneric append (rb-vector val)
  (:documentation
   "Return a new, 1+ sized rb-vector with val at the back"))

(defgeneric count (rb-vector)
  (:documentation
   "Return the number of objects in a rb-vector"))

(declaim (ftype (function (rb-vector) (unsigned-byte 32)) count))

(defgeneric pop (rb-vector)
  (:documentation
   "Return a new, 1- sized rb-vector"))

(defgeneric slice (rb-vector start &optional end)
  (:documentation
   "Return a new rb-vector containing only items indexed from start to end."))

(defgeneric peek (rb-vector)
  (:documentation
   "Return the last element of rb-vector"))

(defgeneric concat (rb-vector1 rb-vector2)
  (:documentation
   "Return a rb-vector created by concatenating two other rb-vectors"))

(defgeneric equal (x y)
  (:documentation
   "Return true if two objects are equal"))

(defstruct (rb-vector (:constructor nil) (:copier nil))
  "Parent type for internal rb-vector implementations, also the API type")

;;; Implementation follows

;; using structs provides a nice performance boost in SBCL: http://www.sbcl.org/manual/index.html#Efficiency

(defstruct (%node (:copier nil)
                  (:conc-name node-)
                  (:constructor make-node))
  "
A node contains an array of either other nodes or values, which forms
a tree - values being the leaves of it.
  "
  (array nil :type (simple-vector 32)))

(defstruct (%rb-vector (:copier nil)
                   (:conc-name rb-vector-)
                   (:constructor make-rb-vector)
                   (:include rb-vector))
  "
A rb-vector is an integer-indexed, immutable collection of any kind of
object.

Shift is the number of bytes to start shifting the 32-bit index by
from when descending down the tree of nodes. To find the next nodes'
index, take the last 5 bits of the shifted number.

Tail is a vector containing the last 32 elements of the rb-vector. Insert,
lookup and append is faster with it because there's no need to traverse
the tree for values in the tail.

Nodes are always (simple-vector 32) because the tail gets copied to a
new node only when it's full. Otherwise new values are inserted to the
tail.

Count is the number of items in the rb-vector, that is, the number of leaf
nodes plus the number of elements in the tail.
  "
  (shift  5 :type (integer 0 30)      :read-only t)
  (root nil :type %node               :read-only t)
  (tail nil :type (simple-vector 32)  :read-only t)
  (count  0 :type (unsigned-byte 32)  :read-only t))

(declaim (inline empty-node))

(defun empty-node ()
  (make-node
   :array
   (make-array 32 :initial-element nil)))

(declaim (inline empty-tail)
         (ftype (function () (simple-vector 32)) empty-tail))

(defun empty-tail ()
  (make-array 32 :initial-element nil))

(declaim (inline empty-rb-vector))

(defun empty-rb-vector ()
  (make-rb-vector
   :root (empty-node)
   :tail (empty-tail)
   :count 0
   :shift 5))

(declaim (inline copy-node))

;; (defun copy-node-array (node)
;;   (declare (optimize speed))
;;   (let ((array (node-array node))
;;         (new-array (make-array 32 :initial-contents
;;     (

(defun copy-node (node)
  (declare (optimize speed))
  (make-node :array (copy-seq (node-array node))))

(declaim (inline copy-tail)
         (ftype (function ((simple-vector 32)) (simple-vector 32)) copy-tail))

(defun copy-tail (tail)
  (declare (optimize speed))
  (copy-seq tail))

(declaim
 (inline nextid)
 (ftype (function ((unsigned-byte 32) &optional (integer 0 30)) (unsigned-byte 5)) nextid))

(defun nextid (index &optional (shift 0))
  (declare (optimize speed))
  (logand (ash index (- shift)) #b11111))

(declaim (inline tree-full-p))

;; rb-vector-full-p
(defun tree-full-p (rb-vector)
  (declare (optimize speed))
  (let ((count (rb-vector-count rb-vector))
        (shift (rb-vector-shift rb-vector)))
    (= count
       (+
       ;; tail is full
        32

        ;; tree is full
        (expt 2 (+ 5 shift))))))

(declaim (inline tail-index-p))
(declaim
 (ftype (function (rb-vector (unsigned-byte 32)) boolean) tail-index-p))

(defun tail-index-p (rb-vector index)
  (declare (optimize speed))
  (let* ((count (rb-vector-count rb-vector))
         ;; Nodes are always full 32 arrays, so this gets the number of
         ;; elements that are outside the nodes, and so, are in the tail.
         (tail-count (1+ (mod (1- count) 32)))
         (threshold (- count tail-count)))
    (or (<= count 32)  ;; needed?
        (>= index threshold))))

(defmethod lookup ((rb-vector %rb-vector) index)
  (declare (optimize speed))
  (if (tail-index-p rb-vector index)
      (svref (rb-vector-tail rb-vector) (nextid index))
      (loop :with shift := (rb-vector-shift rb-vector)
            :with root-array := (node-array (rb-vector-root rb-vector))
            :for level :downfrom shift :above 0 :by 5
            :for nextid := (nextid index level)
            :for arr := (node-array (svref root-array nextid))
            :then (node-array (svref arr nextid))
            :finally (return (svref arr (nextid index))))))

(define-symbol-macro next-node (svref (node-array node) nextid))

(defmethod insert ((rb-vector %rb-vector) index val)
  (declare (optimize speed))
  (if (tail-index-p rb-vector index)
      (let ((tail (copy-tail (rb-vector-tail rb-vector))))
        (setf (aref tail (nextid index)) val)
        ;; Can share everything else
        (make-rb-vector
         :root (rb-vector-root rb-vector)
         :shift (rb-vector-shift rb-vector)
         :count (rb-vector-count rb-vector)
         :tail tail))
      (loop :with root := (copy-node (rb-vector-root rb-vector))
            :with shift := (rb-vector-shift rb-vector)
            :for level :downfrom shift :above 0 :by 5
            :for nextid := (nextid index level)
            ;; copy path from the root down the tree
            :for node := (setf node root next-node (copy-node next-node))
              :then (setf next-node (copy-node next-node))
            :finally
               (setf (aref (node-array node) (nextid index)) val)
               (return (make-rb-vector :shift shift
                                   :root root
                                   :tail (rb-vector-tail rb-vector)
                                   :count (rb-vector-count rb-vector))))))

(declaim (inline tail-full-p))
;; tail-full-p
(defun tail-full-p (rb-vector)
  ;; if count is a multiple of 32, then the tail is full, since it
  ;; gets flushed during every 33th append
  (declare (optimize speed))
  (let ((count (rb-vector-count rb-vector)))
    (and (plusp count)
         (zerop (mod count 32)))))

(declaim (inline rb-vector-push-tail))
;; let  count
(defun rb-vector-push-tail (rb-vector val)
  (declare (optimize speed))
  (let ((tail (copy-tail (rb-vector-tail rb-vector))))
    (setf (svref tail (nextid (rb-vector-count rb-vector))) val)
    (make-rb-vector :shift (rb-vector-shift rb-vector)
                :count (1+ (rb-vector-count rb-vector))
                :root (rb-vector-root rb-vector)
                :tail tail)))

;; do something with the similiarities between the next two

;; FIXME could reuse rb-vector-grow-from-tail
(defun rb-vector-grow-share-root (rb-vector val)
  "Incorporate current tail into node tree, push val to a new tail, increasing tree depth"
  (let ((root (empty-node)))

    ;; share whole thing on the 'left'
    (setf (aref (node-array root) 0) (rb-vector-root rb-vector))

    (loop

          ;; tree and tail are full at this point

          ;; going down the last node (because of shift+5) - NOT the last value
          :with index fixnum := (1- (rb-vector-count rb-vector))

          ;; increase tree depth
          :with shift fixnum := (+ 5 (rb-vector-shift rb-vector))

          :for level fixnum :downfrom shift :above 0 :by 5
          :for nextid fixnum := (nextid index level)

          ;; make a new path to leaf
          :for node := (setf node root next-node (empty-node))
            :then (setf next-node (empty-node))

          :finally

             ;; place the full tail as the value array of the last node (leaves)
             ;; can use reference since everything else does CoW
             (setf (node-array node) (rb-vector-tail rb-vector))

             ;; place the incoming val in a fresh tail
             (let ((tail (empty-tail)))
               (setf (svref tail 0) val)

               (return (make-rb-vector
                        :root root
                        :shift shift
                        :count (1+ (rb-vector-count rb-vector))
                        :tail tail))))))

(declaim (inline make-rb-vector make-node rb-vector-grow-from-tail))

(defun rb-vector-grow-from-tail (rb-vector val)
  "Incorporate current tail into node tree, push val to a new tail"
  (loop :with index := (1- (rb-vector-count rb-vector))
        :with shift := (rb-vector-shift rb-vector)

        ;; Copy root, because we're making a new path to leaf
        :with root := (copy-node (rb-vector-root rb-vector))

        :with tail := (empty-tail)
        :with node := root
        :for level :downfrom shift :above 0 :by 5
        :for nextid := (nextid index level)
        :do (setf next-node
                  (if next-node
                      (copy-node next-node)
                      (empty-node))
                  node next-node)
        :finally
           (setf (svref tail 0) val)
           (setf (node-array node) (rb-vector-tail rb-vector))
           (return
             (make-rb-vector
              :root root
              :shift shift
              :count (1+ (rb-vector-count rb-vector))
              :tail tail))))

(defmethod append ((rb-vector %rb-vector) val)
  (cond ((not (tail-full-p rb-vector))
         (rb-vector-push-tail rb-vector val))
        ((tree-full-p rb-vector)
         (rb-vector-grow-share-root rb-vector val))
        (t
         (rb-vector-grow-from-tail rb-vector val))))

(defmethod count ((rb-vector %rb-vector))
  (rb-vector-count rb-vector))

;;; Slices

(defstruct (%slice (:conc-name slice-)
                   (:constructor make-slice)
                   (:include rb-vector))
  (rb-vector (empty-rb-vector) :type rb-vector :read-only t)
  (start 0 :type (unsigned-byte 32) :read-only t)
  (end 0 :type (unsigned-byte 32) :read-only t))

;; append on slice is increase end and insert there to the backing rb-vector
;; if reached end of backing rb-vector, append on it instead
(defmethod append ((rb-vector %slice) val)
  (let ((end (slice-end rb-vector))
        (backing-rb-vector (slice-rb-vector rb-vector)))
    (make-slice :start (slice-start rb-vector)
                :end (1+ end)
                :rb-vector
                (if (= end (count backing-rb-vector))
                    (append backing-rb-vector val)
                    (insert backing-rb-vector end val)))))

;; insert on slice is just insert on the backing rb-vector with index+start
(defmethod insert ((rb-vector %slice) index val)
  (let ((start (slice-start rb-vector)))
    (declare (type (unsigned-byte 32) start index))
    (make-slice :start start
                :end (slice-end rb-vector)
                :rb-vector (insert (slice-rb-vector rb-vector) (+ index start) val))))

;; count on slice is (- end start)
(defmethod count ((rb-vector %slice))
  (- (slice-end rb-vector) (slice-start rb-vector)))

;; lookup on slice is lookup on backing rb-vector with index+start
(defmethod lookup ((rb-vector %slice) index)
  (let ((start (slice-start rb-vector)))
    (declare (type (unsigned-byte 32) index start))
    (lookup (slice-rb-vector rb-vector) (+ index start))))

;;; Concatenates

(defstruct (%cat (:conc-name cat-)
                 (:constructor make-cat)
                 (:include rb-vector))
  (a (empty-rb-vector) :type rb-vector :read-only t)
  (b (empty-rb-vector) :type rb-vector :read-only t))

(defmethod lookup ((rb-vector %cat) index)
  (let ((a (cat-a rb-vector))
        (b (cat-b rb-vector)))
    (if (< index (count a))
        (lookup a index)
        (lookup b (- index (count a))))))

(defmethod insert ((rb-vector %cat) index val)
  (let ((a (cat-a rb-vector))
        (b (cat-b rb-vector)))
    (if (< index (count a))
        (insert a index val)
        (insert b (- index (count a)) val))))

(defmethod append ((rb-vector %cat) val)
  (append (cat-b rb-vector) val))

(defmethod count ((rb-vector %cat))
  (+ (count (cat-a rb-vector))
     (count (cat-b rb-vector))))

;;; Shared methods

(defmethod peek ((rb-vector rb-vector))
  (lookup rb-vector (1- (count rb-vector))))

(defmethod pop ((rb-vector rb-vector))
  (slice rb-vector 0 (1- (count rb-vector))))

(defmethod slice ((rb-vector rb-vector) start &optional (end (count rb-vector)))
  (make-slice :start start :end end :rb-vector rb-vector))

(defmethod concat ((rb-vector1 rb-vector) (rb-vector2 rb-vector))
  (make-cat :a rb-vector1 :b rb-vector2))

(defmethod remove ((rb-vector rb-vector) index)
  (concat (slice rb-vector 0 index)
          (slice rb-vector (1+ index))))

(defmethod rb-vector (&rest elems)
  "Create a rb-vector containing arbitrary elems"
  (cl:reduce #'append elems :initial-value (empty-rb-vector)))

(defmethod equal ((val1 t) (val2 t))
  "Return true if two vals are cl:equal"
  (cl:equal val1 val2))

(defmethod equal ((rb-vector1 rb-vector) (rb-vector2 rb-vector))
  "Return true if two rb-vectors are equal in size, and their elements, in order, are rb-vector:equal"
  (let ((cnt1 (count rb-vector1))
        (cnt2 (count rb-vector2)))
    (declare (type fixnum cnt1 cnt2))
    (when (= cnt1 cnt2)
      (loop for x fixnum below cnt1
            always (equal (lookup rb-vector1 x)
                          (lookup rb-vector2 x))))))

(defmethod print-object ((object rb-vector) stream)
  (declare (type stream stream))
  (print-unreadable-object (object stream :type t)
    (loop :for i fixnum :below (count object)
          :do (format stream "~s " (lookup object i)))))
