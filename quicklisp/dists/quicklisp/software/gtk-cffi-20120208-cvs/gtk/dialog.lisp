(in-package :gtk-cffi)

(defclass dialog (window)
  ())

(defbitfield dialog-flags
  :modal
  :destroy-with-parent
  :no-separator)

(defcenum dialog-response
  (:help -11)
  :apply :no :yes :close :cancel :ok :delete :accept :reject :none)

(defcfun "gtk_dialog_new_with_buttons"
  :pointer (title gtk-string)
  (parent pobject) (flags dialog-flags) (null :pointer))

(defcfun "gtk_dialog_new" :pointer)

(defmethod gconstructor ((dialog dialog)
                         &key title parent (flags 0) &allow-other-keys)
  (if title
      (gtk-dialog-new-with-buttons title parent flags (null-pointer))
    (gtk-dialog-new)))
                         
(defmethod initialize-instance
  :after ((dialog dialog)
          &key with-buttons &allow-other-keys)        
  (mapcar
   (lambda (x)
     (destructuring-bind (str resp) x
       (add-button dialog str resp)))
   with-buttons))
  

(defcfun "gtk_dialog_run" dialog-response (dialog :pointer))

(defmethod run ((dialog dialog) &key (keep-alive t))
  (let ((resp (gtk-dialog-run (pointer dialog))))
    (unless keep-alive
      (destroy dialog))
    resp))

(defcfun "gtk_dialog_add_button" pobject (dialog pobject)
  (str gtk-string) (resp dialog-response))

(defmethod add-button ((dialog dialog) str response)
  (gtk-dialog-add-button dialog (if (keywordp str) (string-downcase str) str)
                         response))

(defcfun gtk-dialog-response :void (dialog pobject) (resp dialog-response))

(defmethod response ((dialog dialog) response)
  (gtk-dialog-response dialog response))

(defcfun gtk-dialog-add-action-widget 
    :void (dialog pobject) (child pobject) (resp dialog-response))

(defmethod add-action-widget ((dialog dialog) (child widget) response)
  (gtk-dialog-add-action-widget dialog child response))

(defcfun gtk-dialog-set-default-response 
    :void (dialog pobject) (resp dialog-response))

(defmethod (setf default-response) (response (dialog dialog))
  (gtk-dialog-set-default-response dialog response))

(defcfun gtk-dialog-set-response-sensitive
    :void (dialog pobject) (resp dialog-response) (setting :boolean))

(defmethod (setf response-sensitive) (setting (dialog dialog) response)
  (gtk-dialog-set-response-sensitive dialog response setting))

(defcfun gtk-dialog-get-response-for-widget 
    dialog-response (dialog pobject) (widget pobject))

(defmethod response-for-widget ((dialog dialog) (widget widget))
  (gtk-dialog-get-response-for-widget dialog widget))

(defcfun gtk-dialog-get-widget-for-response 
    pobject (dialog pobject) (response dialog-response))

(defmethod widget-for-response ((dialog dialog) response)
  (gtk-dialog-get-widget-for-response dialog response))

(defcfun gtk-dialog-get-action-area pobject (dialog pobject))

(defmethod action-area ((dialog dialog))
  (gtk-dialog-get-action-area dialog))

(defcfun gtk-dialog-get-content-area pobject (dialog pobject))

(defmethod content-area ((dialog dialog))
  (gtk-dialog-get-content-area dialog))

(defcfun gtk-alternative-dialog-button-order :boolean (screen pobject))

(defmethod alternative-dialog-button-order ((screen screen))
  (gtk-alternative-dialog-button-order screen))

(defcfun gtk-dialog-set-alternative-button-order-from-array
    :void (dialog pobject) (n-params :int) (new-order :pointer))

(defmethod (setf alternative-button-order) (order (dialog dialog))
  (let ((n-params (length order)))
    (with-foreign-object (arr :int n-params)
      (loop 
         :for i :from 0 :to n-params
         :for l :in order
         :do (setf (mem-aref arr :int i) l))
      (gtk-dialog-set-alternative-button-order-from-array dialog 
                                                          n-params arr))))