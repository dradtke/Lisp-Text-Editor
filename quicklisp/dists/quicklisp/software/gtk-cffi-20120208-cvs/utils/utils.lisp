(in-package :gtk-cffi-utils)

(defmacro debug-out (&body body)
;  (declare (ignore body))
  `(format t ,@body)
  )

(defmacro memo (place &body body)
  `(or ,place
       (setf ,place ,@body)))

(defun find-key (key seq)
  (when seq
    (if (eq key (car seq)) 
        (list (first seq) (second seq))
        (find-key key (cddr seq)))))

(defmacro with-hash (hash key &body body)
  "If found KEY in HASH, return corresponding value,
else use BODY to calculate the value and save to HASH.
NIL values not saved"
  (let ((try (gensym)))
    `(or (gethash ,key ,hash)
         (let ((,try (progn ,@body)))
           (when ,try
             (setf (gethash ,key ,hash) ,try))))))

(defmacro bitmask (&rest flags)
  "Returns list from lisp values as keywords:
 Example: (bitmask after swapped)
 -> nil, when after=nil and swapped=nil
 -> (:after), when after=t and swapped=nil
 -> (:swapped), when after=nil and swapped=t
 -> (:after :swapped), when both are t"
  `(flatten
    (list ,@(iter
             (for flag in flags)
             (collect `(when ,flag
                         ,(make-keyword flag)))))))

(defmacro template (args &body body)
  (with-gensyms (%template %do)
    `(macrolet ((,%do ()
                  (flet ((,%template (param) ,@body))
                    `(progn
                       ,@(mapcar #',%template ',args)))))
       (,%do))))

