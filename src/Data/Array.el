;; -*- lexical-binding: t; -*-

;; ref
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html

(defvar Data.Array.fromFoldableImpl
  (lambda (foldr)
    (lambda (xs)
      (apply 'vector
             (psel/funcall
              foldr
              (lambda (a) (lambda (b) (cons a b)))
              nil
              xs)))))

(defvar Data.Array.range
  (lambda (start)
    (lambda (end)
      (let* ((step (if (> start end) -1 1))
             (result (make-vector (1+ (* step (- end start))) nil))
             (v start)
             (i 0))
        (while (not (= v end))
          (aset result i v)
          (setq i (1+ i)
                v (+ v step)))
        (aset result i v)
        result))))

(defvar Data.Array.replicate
  (lambda (count)
    (lambda (value)
      (if (< 0 count)
          (make-vector count value)
        (make-vector 0 nil)))))

(defvar Data.Array.length
  (lambda (xs)
    (length xs)))

(defvar Data.Array.unconsImpl
  (lambda (empty)
    (lambda (next)
      (lambda (xs)
        (if (= 0 (length xs))
            (funcall empty Data.Unit.unit)
           (psel/funcall next (aref xs 0) (seq-drop xs 1)))))))

(defvar Data.Array.indexImpl
  (lambda (just)
    (lambda (nothing)
      (lambda (xs)
        (lambda (i)
          (if (and (<= 0 i) (< i (length xs)))
              (funcall just (aref xs i))
            nothing))))))

(defvar Data.Array.findMapImpl
  (lambda (nothing)
    (lambda (isJust)
      (lambda (f)
        (lambda (xs)
          (let ((i 0)
                (len (length xs))
                (result nil))
            (while (and (null result) (< i len))
              (let ((v (funcall f (aref xs i))))
                (when (funcall isJust v)
                  (setq result v))
                (setq i (1+ i))))
            (if result result nothing)))))))

(defvar Data.Array.findIndexImpl
  (lambda (just)
    (lambda (nothing)
      (lambda (f)
        (lambda (xs)
          (let ((len (length xs))
                (v nil)
                (i 0))
            (while (and (null v) (< i len))
              (when (funcall f (aref xs i))
                (setq v i))
              (setq i (1+ i)))
            (if v (funcall just v) nothing)))))))

(defvar Data.Array.findLastIndexImpl
  (lambda (just)
    (lambda (nothing)
      (lambda (f)
        (lambda (xs)
          (let* ((len (length xs))
                 (v nil)
                 (i (1- len)))
            (while (and (null v) (<= 0 i))
              (when (funcall f (aref xs i))
                (setq v i))
              (setq i (1- i)))
            (if v (funcall just v) nothing)))))))

;; i の上限チェックが <= であることに注意
(defvar Data.Array._insertAt
  (lambda (just)
    (lambda (nothing)
      (lambda (i)
        (lambda (a)
          (lambda (xs)
            (if (and (<= 0 i) (<= i (length xs)))
                (funcall just
                         (vconcat (seq-take xs i)
                                  (vector a)
                                  (seq-drop xs i)))
              nothing)))))))

(defvar Data.Array._deleteAt
  (lambda (just)
    (lambda (nothing)
      (lambda (i)
        (lambda (xs)
          (if (and (<= 0 i) (< i (length xs)))
              (funcall just
                       (vconcat (seq-take xs i)
                                (seq-drop xs (1+ i))))
            nothing))))))

(defvar Data.Array._updateAt
  (lambda (just)
    (lambda (nothing)
      (lambda (i)
        (lambda (a)
          (lambda (xs)
            (if (and (<= 0 i) (< i (length xs)))
                (let ((ys (seq-copy xs)))
                  (aset ys i a)
                  (funcall just ys))
              nothing)))))))

(defvar Data.Array.reverse
  (lambda (xs)
    (seq-reverse xs)))

(defvar Data.Array.concat
  (lambda (xss)
    (apply 'vconcat (seq-into xss 'list))))

(defvar Data.Array.filter
  (lambda (f)
    (lambda (xs)
      (seq-filter (lambda (a) (funcall f a)) xs))))

(defvar Data.Array.partition
  (lambda (f)
    (lambda (xs)
      (let* ((v (seq-group-by (lambda (a) (funcall f a)) xs))
             (yes (seq-into (alist-get t v) 'vector))
             (no (seq-into (alist-get nil v) 'vector)))
        `((yes . ,yes) (no . ,no))))))

(defvar Data.Array.scanl
  (lambda (f)
    (lambda (b)
      (lambda (xs)
        (let* ((len (length xs))
               (acc b)
               (out (make-vector len nil))
               (i 0))
          (while (< i len)
            (setq acc (psel/funcall f acc (aref xs i)))
            (aset out i acc)
            (setq i (1+ i)))
          out)))))

(defvar Data.Array.scanr
  (lambda (f)
    (lambda (b)
      (lambda (xs)
        (let* ((len (length xs))
               (acc b)
               (out (make-vector len nil))
               (i (1- len)))
          (while (<= 0 i )
            (setq acc (psel/funcall f (aref xs i) acc))
            (aset out i acc)
            (setq i (1- i)))
          out)))))

(defvar Data.Array.sortByImpl
  (lambda (compare)
    (lambda (fromOrdering)
      (lambda (xs)
        (seq-sort
         (lambda (a b) (< 0 (funcall fromOrdering (psel/funcall compare a b))))
         xs)))))

(defvar Data.Array.slice
  (lambda (s)
    (lambda (e)
      (lambda (xs)
        (seq-subseq xs s e)))))

(defvar Data.Array.zipWith
  (lambda (f)
    (lambda (xs)
      (lambda (ys)
        (seq-mapn (lambda (x y) (psel/funcall f x y))
                  xs
                  ys)))))

(defvar Data.Array.any
  (lambda (p)
    (lambda (xs)
      (let ((len (length xs))
            (i 0)
            (v nil))
        (while (and (not v) (< i len))
          (setq v (funcall p (aref xs i)))
          (setq i (1+ i)))
        v))))

(defvar Data.Array.all
  (lambda (p)
    (lambda (xs)
      (seq-every-p (lambda (x) (funcall p x)) xs))))

(defvar Data.Array.unsafeIndexImpl
  (lambda (xs)
    (lambda (i)
      (aref xs i))))
