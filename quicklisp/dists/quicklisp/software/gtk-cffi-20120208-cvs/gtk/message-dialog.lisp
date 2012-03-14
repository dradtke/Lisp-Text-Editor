(in-package :gtk-cffi)

(defclass message-dialog (dialog)
  ())

(defcenum buttons-type
  :none :ok :close :cancel :yes-no :ok-cancel)

(defcenum message-type
  :info :warning :question :error :other)

(defcfun "gtk_message_dialog_new" :pointer (parent pobject)
  (flags dialog-flags) (type message-type) (buttons buttons-type)
  (message gtk-string) (null :pointer))

(defcfun "gtk_message_dialog_new_with_markup" :pointer (parent pobject)
  (flags dialog-flags) (type message-type) (buttons buttons-type)
  (message gtk-string) (null :pointer))


(defmethod gconstructor ((message-dialog message-dialog)
                         &key parent (flags 0)
                         (type :info) (buttons :ok)
                         (message "") markup &allow-other-keys)
  (funcall
   (if markup #'gtk-message-dialog-new-with-markup
     #'gtk-message-dialog-new)
   parent flags type buttons message (null-pointer)))

(defun show-message (parent message &key (type :info) (buttons :ok) markup)
  (run (make-instance 'message-dialog :parent parent
                      :message message
                      :type type :buttons buttons :markup markup) 
       :keep-alive nil))
