;; -*- lexical-binding: t; -*-

(defvar Data.Array.NonEmpty.Internal.foldr1Impl
  (lambda (f)
    (lambda (xs)
      (let* ((len (length xs))
             (acc (aref xs (1- len)))
             (i (- len 2)))
        (while (<= i)
          (setq acc (psel/funcall f (aref xs i) acc))
          (setq i (1- i)))
        acc))))

(defvar Data.Array.NonEmpty.Internal.foldl1Impl
  (lambda (f)
    (lambda (xs)
      (let* ((len (length xs))
             (acc (aref xs 0))
             (i 1))
        (while (< i len)
          (setq acc (psel/funcall f acc (aref xs i)))
          (setq i (1+ i)))
        acc))))

;; foldable-traversableライブラリの Data.Traversal.traverseArrayImplのシンプル版。
;; Data.Traversal.traverseArrayImplの方は分割統治使っているが、こちらのはJS版と合わせて
;; シンプルな実装に留める。
(defvar Data.Array.NonEmpty.Internal.traverse1Impl
  (lambda (apply)
    (lambda (map)
      (lambda (f)
        (lambda (xs)
          (let ((consf (funcall map (lambda (a) (lambda (b) (cons a b)))))
                (acc (psel/funcall map (lambda (x) (list x)) (funcall f (aref xs 0))))
                (len (length xs))
                (i 1))
            (while (< i len)
              (setq acc (psel/funcall apply
                                      (funcall consf (funcall f (aref xs i)))
                                      acc))
              (setq i (1+ i)))
            (psel/funcall map
                          (lambda (ys) (apply 'vector (reverse ys)))
                          acc)))))))
            ;; (psel/funcall map
            ;;               (lambda (ys) (apply 'vector (reverse ys)))
            ;;               (seq-reduce (lambda (b a)
            ;;                             (let* ((mb (funcall f a))
            ;;                                    (mc (psel/funcall map cons_ mb)))
            ;;                               (psel/funcall apply mc b)))
            ;;                           xs
            ;;                           init)))))))))
