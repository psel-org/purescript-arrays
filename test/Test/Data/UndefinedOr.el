;; -*- lexical-binding: t; -*-

(defvar Test.Data.UndefinedOr.undefined
  nil)

(defvar Test.Data.UndefinedOr.defined
  (lambda (a)
    (cons a nil)))

(defvar Test.Data.UndefinedOr.eqUndefinedOrImpl
  (lambda (eq)
    (lambda (a)
      (lambda (b)
        (or (and (null a) (null b))
            (and (not (null a))
                 (not (null b))
                 (psel/funcall eq (car a) (car b))))))))

(defvar Test.Data.UndefinedOr.compareUndefinedOrImpl
  (lambda (lt)
    (lambda (eq)
      (lambda (gt)
        (lambda (compare)
          (lambda (a)
            (lambda (b)
              (cond ((and (null a) (null b)) eq)
                    ((null a) lt)
                    ((null b) gt)
                    (t (psel/funcall compare (car a) (car b)))))))))))
