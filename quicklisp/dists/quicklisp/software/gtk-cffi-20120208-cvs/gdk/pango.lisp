(defpackage #:pango-cffi
  (:use #:common-lisp #:cffi-object #:cffi #:iterate #:g-object-cffi
        #:alexandria #:gtk-cffi-utils)
  (:export
   #:font
   #:tab-array
   #:language
   #:alignment
   #:ellipsize-mode
   #:stretch
   #:style
   #:underline
   #:variant
   #:wrap-mode
   #:direction
   #:attr-list))

(in-package #:pango-cffi)

(register-package "Pango" *package*)
(register-prefix *package* 'pango)


(defcfun ("pango_font_description_from_string" string->pango-font)
  :pointer (str gtk-string))

(defcfun ("pango_font_description_to_string" pango-font->string)
  gtk-string (font :pointer))

(defcfun pango-font-description-free :void (font :pointer))

(define-foreign-type font (freeable)
  ()
  (:actual-type :pointer)
  (:simple-parser font))

(defmethod free-ptr ((type font) ptr)
  (pango-font-description-free ptr))

(defmethod translate-to-foreign (value (type font))
  (string->pango-font value))

(defmethod translate-from-foreign (ptr (type font))
  (unless (null-pointer-p ptr)
    (pango-font->string ptr)))

(defcenum alignment
  :left :center :right)

(defcenum ellipsize-mode
  :none :start :middle :end)

(defcenum stretch
  :ultra-condensed
  :extra-condensed
  :dcondensed
  :semi-condensed
  :normal
  :semi-expanded
  :expanded
  :extra-expanded
  :ultra-expanded)

(defcenum style
  :normal :oblique :italic)

(defcenum underline
  :none :single :double :low :error)

(defcenum variant
  :normal :small-caps)

(defcenum wrap-mode
  :word :char :word-char)

(defcenum direction
  :ltr :rtl :ttb-ltr :ttb-rtl :weak-ltr :weak-rtl :neutral)

(defcenum gravity
  :south :east :north :west :auto)

(defcenum gravity-hint
  :natural :strong :line)

(defcenum weight
  (:thin 100)
  (:ultralight 200)
  (:light 300)
  (:book 380)
  (:normal 400)
  (:medium 500)
  (:semibold 600)
  (:bold 700)
  (:ultrabold 800)
  (:heavy 900)
  (:ultraheavy 1000))

(define-foreign-type tab-array (freeable)
  ()
  (:actual-type :pointer))

(define-parse-method tab-array (&key free)
  (make-instance 'tab-array :free free))

;; We need to pass positions-in-pixels (boolean) and list of tab-stops
;; in lisp it is handy to represent as (pixels {tab-stop}*), where
;; pixels is t or nil and tab-stop is a fixnum

(defcenum tab-align :left)

(defcfun pango-tab-array-new :pointer (size :int) (pixels :boolean))
(defcfun pango-tab-array-set-tab :void
  (tab-array :pointer) (index :int) (alignment tab-align) (location :int))
(defcfun pango-tab-array-get-size :int (tab-array :pointer))
(defcfun pango-tab-array-get-tab :void
  (tab-array :pointer) (index :int) (alignment :pointer) (location :pointer))
(defcfun pango-tab-array-get-positions-in-pixels :boolean (tab-array :pointer))
(defcfun pango-tab-array-free :void (tab-array :pointer))

(defmethod free-ptr ((type tab-array) ptr)
  (pango-tab-array-free ptr))


(defmethod translate-to-foreign (value (type tab-array))
  "VALUE should be (pixels {tab-stop}*)
pixels = {t = the tab positions are in pixels} or {nil = in Pango units}
tab-stop = fixnum or (align . location), where location is fixnum
           and align is a tab-align"
  (let* ((l (length (cdr value)))
         (res (pango-tab-array-new (car value) l)))
    (iter (for tab-stop in (cdr value))
          (for index from 0 to l)
          (etypecase tab-stop
            (cons (pango-tab-array-set-tab res index 
                                           (car tab-stop) (cdr tab-stop)))
            (fixnum (pango-tab-array-set-tab res index 0 tab-stop))))
    res))

;(defmethod free-translated-object (value (type tab-array) param)
;  (declare (ignore param))
;  (pango-tab-array-free value))

(defmethod translate-from-foreign (ptr (type tab-array))
  (unless (null-pointer-p ptr)
    (cons (pango-tab-array-get-positions-in-pixels ptr)
          (iter (for index from 0 below (pango-tab-array-get-size ptr))
                (collect
                    (destructuring-bind (alignment location)
                        (with-foreign-outs ((alignment 'tab-align)
                                            (location :int)) :ignore
                          (pango-tab-array-get-tab ptr index 
                                                   alignment location))
                      (if (eq alignment :left) 
                          location
                          (cons alignment location))))))))


(defctype language :pointer)
;; for language we don't need foreign type, because we don't need
;; to free these pointers for languages
(defcfun (string->language "pango_language_from_string") language 
  (str gtk-string))
(defcfun (language->string "pango_language_to_string") gtk-string 
  (language language))

(eval-when (:compile-toplevel :load-toplevel)
  (defcenum attr-type
    :invalid :language :family :style :weight :variant :stretch :size
    :font-desc :foreground :background :underline :strikethrough
    :rise :shape :scale :fallback :letter-spacing :underline-color
    :strikethrough-color :absolute-size :gravity :gravity-hint))

(defcstruct attribute
  (klass (:pointer attr-type))
  (start-index :uint)
  (end-index :uint))

(defcstruct attr-string
  (attr attribute)
  (value :string))

(defcstruct attr-language
  (attr attribute)
  (value language))

(defcstruct color
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(defcstruct attr-color
  (attr attribute)
  (value color))

(defcstruct attr-int
  (attr attribute)
  (value :int))

(defcstruct attr-float
  (attr attribute)
  (value :float))

(defcstruct attr-font-desc
  (attr attribute)
  (value font))

(defcstruct rectangle
  (x :int) (y :int)
  (width :int) (height :int))

(defcstruct attr-shape
  (attr attribute)
  (ink rectangle)
  (logical rectangle)
  (data :pointer)
  (copy-func :pointer)
  (destroy-func :pointer))

(defcstruct attr-size
  (attr attribute)
  (size :int)
  (absolute :uint))

(defun rect->list (rect)
  (with-foreign-slots ((x y width height) rect rectangle)
    (list x y width height)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun attr->type (ktype)
    (ecase ktype
      (:language 'attr-language)
      (:family 'attr-string)
      ((:style :weight :variant :stretch
               :underline :strikethrough
               :rise :fallback :letter-spacing
               :gravity :gravity-hint) 'attr-int)
      ((:size :absolute-size) 'attr-size)
      (:font-desc 'attr-font-desc)
      (:shape 'attr-shape)
      (:scale 'attr-float)
      ((:foreground :background 
                    :underline-color 
                    :strikethrough-color) 'attr-color))))
  
(defun translate-to-enum (type value)
  (case type
    ((:style :weight :variant :stretch :underline :gravity :gravity-hint)
     (convert-from-foreign value (intern (symbol-name type) #.*package*)))
    ((:strikethrough :fallback) (convert-from-foreign value :boolean))
    (t value)))
    

(defun attr->list (attr)
  (let* ((type (mem-ref (foreign-slot-value attr 'attribute 'klass) 
                        'attr-type))
         (tail-type (attr->type type)))
    (with-foreign-slots ((start-index end-index) attr attribute)
      (list* type start-index end-index
             (ecase tail-type
               ((attr-language attr-string attr-font-desc attr-float)
                (list (foreign-slot-value attr tail-type 'value)))
               (attr-int (list (translate-to-enum
                                type
                                (foreign-slot-value attr tail-type 'value))))
               (attr-color (with-foreign-slots 
                               ((red green blue) 
                                (foreign-slot-value attr 'attr-color 'value)
                                color)
                             (list red green blue)))
                             
               (attr-size (list (foreign-slot-value attr tail-type 'size)))
               (attr-shape
                (with-foreign-slots ((ink logical) attr attr-shape)
                  (list (rect->list ink) (rect->list logical)))))))))



(template (:language :family :style :variant :stretch :weight :size
                     :font-desc :strikethrough :underline :scale
                     :rise :letter-spacing :fallback :gravity
                     :gravity-hint)
  (flet ((in-type (type)
           (case type
             (:family :string)
             ((:size :rise :letter-spacing) :int)
             (:font-desc 'font)
             ((:strikethrough :fallback) :boolean)
             (:scale :double)
             (t (intern (symbol-name type) #.*package*)))))
    `(defcfun ,(symbolicate 'pango-attr- param '-new) ,(attr->type param)
       (value ,(in-type param)))))

(template (:foreground :background :strikethrough-color :underline-color)
  `(defcfun ,(symbolicate 'pango-attr- param '-new) attr-color
     (red :uint16) (green :uint16) (blue :uint16)))

(defcfun ("pango_attr_size_new_absolute" pango-attr-absolute-size-new) 
    attr-size (size :int))

(define-foreign-type rect-list (freeable)
  ()
  (:simple-parser rect-list)
  (:actual-type :pointer))

(defmethod translate-to-foreign (value (type rect-list))
  (let ((ptr (foreign-alloc 'rectangle)))
    (with-foreign-slots ((x y width height) ptr rectangle)
      (destructuring-bind (new-x new-y new-width new-height) value
        (setf x new-x
              y new-y
              width new-width
              height new-height)))
    ptr))


(defcfun pango-attr-shape-new attr-shape (ink rect-list) (logical rect-list))
               
(define-foreign-type attr-list (freeable)
  ((free :initform t))
  (:simple-parser attr-list)
  (:actual-type :pointer))

;; (deffuns attr-list
;;   (ref :pointer)
;;   (unref :void)
;;   (filter :pointer (func :pointer) (data :pointer)))

(defcfun pango-attr-list-unref :void (ptr :pointer))
(defcfun pango-attr-list-filter :pointer 
  (ptr :pointer) (func :pointer) (data :pointer))

(defmethod free-ptr ((type attr-list) ptr)
  (pango-attr-list-unref ptr))

(defvar *attr-list* nil)

(defcallback cb-attr-list :boolean ((pattr :pointer) (data :pointer))
  (declare (ignore data))
  (push (attr->list pattr) *attr-list*)
  t)

(defmethod translate-from-foreign (ptr (type attr-list))
  (let (*attr-list*)
    (pango-attr-list-filter ptr (callback cb-attr-list) (null-pointer))
    *attr-list*))

(defcfun pango-attr-list-new :pointer)
(defcfun pango-attr-list-insert :void (list :pointer) (attr :pointer))

(template (t)
  (declare (ignore param))
  `(defun list->attr (l)
     (destructuring-bind (type start-index end-index &rest params) l
       (let ((ptr
              (apply
               (case type
                 ,@(mapcar (lambda (x) `(,x 
                                         (function ,(symbolicate 
                                                     'pango-attr- x '-new))))
                           (cdr (foreign-enum-keyword-list 'attr-type))))
               params)))
         (setf (foreign-slot-value ptr 'attribute 'start-index) start-index
               (foreign-slot-value ptr 'attribute 'end-index) end-index)
         ptr))))
         

(defmethod translate-to-foreign (value (type attr-list))
  (let ((ptr (pango-attr-list-new)))
    (mapc (lambda (x) (pango-attr-list-insert ptr (list->attr x)))
          value)
    ptr))