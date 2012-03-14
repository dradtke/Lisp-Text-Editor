(in-package :gtk-cffi)

(defclass label (misc)
  ())

(defcenum justification
  :left :right :center :fill)

(defcfun "gtk_label_new" :pointer (text gtk-string))

(defmethod gconstructor ((label label)
                         &key text &allow-other-keys)
  (gtk-label-new text))

(defcfun "gtk_label_set_markup" :void (label pobject) (text gtk-string))

(defcfun "gtk_label_set_markup_with_mnemonic"
  :void (label pobject) (text gtk-string))

(defcfun "gtk_label_set_text_with_mnemonic"
  :void (label pobject) (text gtk-string))

(defcfun "gtk_label_set_text"
  :void (label pobject) (text gtk-string))

(defmethod (setf text) (text (label label) &key mnemonic markup)
  (apply
   (if mnemonic
       (if markup
           #'gtk-label-set-markup-with-mnemonic
         #'gtk-label-set-text-with-mnemonic)
     (if markup #'gtk-label-set-markup
       #'gtk-label-set-text))
   (list label text)))

(defcfun "gtk_label_get_text" gtk-string (label pobject))

(defcfun "gtk_label_get_label" gtk-string (label pobject))

(defmethod text ((label label) &key markup)
  (apply
   (if markup #'gtk-label-get-label
     #'gtk-label-get-text) label))

(defslots label
  mnemonic-widget pobject
  justify justification)


;; taken from cells-gtk
(defun to-str (sym)
  (if (stringp sym)
      sym
      (string-downcase (format nil "~a" sym))))

(defmacro with-markup (markup &rest rest)
  (destructuring-bind (&key font-desc font-family face size style
                weight variant stretch foreground background
                underline rise strikethrough fallback lang) markup
    (let ((markup-start
       `(format nil "<span~{ ~a=~s~}>"
         (list
          ,@(when font-desc `("font_desc" (to-str ,font-desc)))
          ,@(when font-family `("font_family" (to-str ,font-family)))
          ,@(when face `("face" (to-str ,face)))
          ,@(when size `("size" (to-str ,size)))
          ,@(when style `("style" (to-str ,style)))
          ,@(when weight `("weight" (to-str ,weight)))
          ,@(when variant `("variant" (to-str ,variant)))
          ,@(when stretch `("stretch" (to-str ,stretch)))
          ,@(when foreground `("foreground" (to-str ,foreground)))
          ,@(when background `("background" (to-str ,background)))
          ,@(when underline `("underline" (to-str ,underline)))
          ,@(when rise `("rise" (to-str ,rise)))
          ,@(when strikethrough `("strikethrough" 
                                  (if ,strikethrough "true" "false")))
          ,@(when fallback `("fallback" (to-str ,fallback)))
          ,@(when lang `("lang" (to-str ,lang)))))))

      `(format nil "~a ~a </span>" ,markup-start (format nil "~{~a~}" 
                                                         (list ,@rest))))))
