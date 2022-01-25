;; -*- lexical-binding: t; -*-

(defvar Data.Array.ST.Partial.peekImpl
  (lambda (i)
    (lambda (sa)
      (lambda ()
        (aref (car sa) i)))))

(defvar Data.Array.ST.Partial.pokeImpl
  (lambda (i)
    (lambda (a)
      (lambda (sa)
        (lambda ()
          (aset (car sa) i a)
          Data.Unit.unit)))))
