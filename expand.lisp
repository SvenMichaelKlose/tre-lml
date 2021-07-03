(var *lml-expander* (define-expander 'LML))

(defmacro define-lml-macro (&rest x)
  (print-definition `(define-lml-macro ,x. ,.x.))
  `(def-expander-macro *lml-expander* ,@x))

(fn lml-expand (x)
  (expander-expand *lml-expander* x))


(define-lml-macro %hook (event-names handler child)
  `(%exec ,#'((parent child)
               (@ (i (ensure-list event-names))
                 (child.add-event-listener i handler)))
     ,child))

(var *lml-hook-attrs* (@ [. (make-keyword (upcase (+ "on-" _))) _] *all-events*))

(fn lml-hook-attr? (x)
  (assoc-value x *lml-hook-attrs*))

(fn lml-hook (x)
  (? (atom x)
     x
     (with-queue q
       (with (f  [?
                   (atom _)
                     _
                   (keyword? _.)  (!? (lml-hook-attr? _.)
                                      (progn
                                        (enqueue q !)
                                        (enqueue q ._.)
                                        (f .._))
                                      (. _. (. ._. (f .._))))
                   (cons? _.)     (. (lml-hook _.) (f ._))
                   (. _. (f ._))]
               m  #'((x elm)
                      (? x
                         `(%hook ,x. ,.x. ,(m ..x elm))
                         elm)))
         (let other-args (f .x)
           (!? (queue-list q)
               (m ! `(,x. ,@other-args))
               `(,x. ,@other-args)))))))


(fn $$ (x &key (parent nil))
  (lml2dom (lml-expand (lml-hook (lml-expand x))) :parent parent))
