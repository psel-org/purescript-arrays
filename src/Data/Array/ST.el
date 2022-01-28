;; -*- lexical-binding: t; -*-

;; emacs では長さを変更できない。
;; そのためconsセルのcarに保持する。

(defvar Data.Array.ST.unsafeFreeze
  (lambda (sa)
    (lambda ()
      (car sa))))

(defvar Data.Array.ST.unsafeThaw
  (lambda (xs)
    (lambda ()
      (cons xs nil))))

(defvar Data.Array.ST.new
  (lambda ()
    (cons (make-vector 0 nil) nil)))

(defvar Data.Array.ST.thaw
  (lambda (xs)
    (lambda ()
      (cons (seq-copy xs) nil))))

(defvar Data.Array.ST.shiftImpl
  (lambda (just)
    (lambda (nothing)
      (lambda (sa)
        (lambda ()
          (if (seq-empty-p (car sa))
              nothing
            (let* ((xs (car sa))
                   (a (aref xs 0)))
              (setcar sa (seq-subseq xs 1))
              (funcall just a))))))))


(defvar Data.Array.ST.sortByImpl
  (lambda (compare)
    (lambda (fromOrdering)
      (lambda (sa)
        (lambda ()
          (setcar sa
                  (seq-sort
                   (lambda (a b) (> 0 (funcall fromOrdering (psel/funcall compare a b))))
                   (car sa)))
          sa)))))

(defvar Data.Array.ST.freeze
  (lambda (sa)
    (lambda ()
      (seq-copy (car sa)))))

(defvar Data.Array.ST.peekImpl
  (lambda (just)
    (lambda (nothing)
      (lambda (i)
        (lambda (sa)
          (lambda ()
            (let ((xs (car sa)))
              (if (and (<= 0 i) (< i (length xs)))
                  (funcall just (aref xs i))
                nothing))))))))

(defvar Data.Array.ST.poke
  (lambda (i)
    (lambda (a)
      (lambda (sa)
        (lambda ()
          (let* ((xs (car sa))
                 (ret (and (<= 0 i) (< i (length xs)))))
            (when ret
              (aset xs i a))
            ret))))))

(defvar Data.Array.ST.popImpl
  (lambda (just)
    (lambda (nothing)
      (lambda (sa)
        (lambda ()
          (if (seq-empty-p (car sa))
              nothing
            (let* ((xs (car sa))
                   (a (aref xs (1- (length xs)))))
              (setcar sa (seq-subseq xs 0 (1- (length xs))))
              (funcall just a))))))))

(defvar Data.Array.ST.pushAll
  (lambda (as)
    (lambda (sa)
      (lambda ()
        (let ((xs (vconcat (car sa) as)))
          (setcar sa xs)
          (length xs))))))

(defvar Data.Array.ST.unshiftAll
  (lambda (as)
    (lambda (sa)
      (lambda ()
        (let ((xs (vconcat as (car sa))))
          (setcar sa xs)
          (length xs))))))

(defvar Data.Array.ST.splice
  (lambda (i)
    (lambda (howMany)
      (lambda (bs)
        (lambda (sa)
          (lambda ()
            (let* ((xs (car sa))
                   (a (seq-take xs i))
                   (b (seq-take (seq-drop xs i) howMany))
                   (c (seq-drop xs (+ i howMany))))
              (setcar sa (vconcat a bs c))
              b)))))))

(defvar Data.Array.ST.toAssocArray
  (lambda (sa)
    (lambda ()
      (let ((xs (car sa)))
        (seq-into
         (seq-map-indexed (lambda (e i) `((value . ,e) (index . ,i))) xs)
         'vector)))))

