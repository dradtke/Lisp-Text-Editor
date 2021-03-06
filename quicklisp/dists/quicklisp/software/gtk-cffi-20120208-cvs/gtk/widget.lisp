;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; widget.asd --- Wrapper for GtkWidget
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package :gtk-cffi)

(defclass widget (g-object)
  ((%style-properties :accessor %style-properties 
                      :initform nil :allocation :class)))

(defclass requisition (struct)
  ())

(defcfun gtk-requisition-new :pointer)

(defmethod new-struct ((class (eql 'requisition)))
  (gtk-requisition-new))

(defcfun gtk-requisition-free :void (requisition pobject))

(defmethod free-struct ((class (eql 'requisition)) value)
  (gtk-requisition-free value))

(defcstruct* requisition
  "GtkRequisition"
  (width :int)
  (height :int))

(init-slots requisition)

(defclass allocation (struct)
  ())

(defcstruct* allocation
  "GtkAllocation"
  (x :int) (y :int)
  (width :int) (height :int))

(init-slots allocation)


(defcfun gtk-widget-show :boolean (widget pobject))
(defcfun gtk-widget-show-all :boolean (widget pobject))
(defcfun gtk-widget-show-now :boolean (widget pobject))

(defgeneric show (widget &key all now) 
  (:documentation "gtk_widget_show[_now|_all] ALL and NOW are booleans"))
(defmethod show ((widget widget) &key (all t) now)
  (funcall (cond 
             (now #'gtk-widget-show-now)
             (all #'gtk-widget-show-all)
             (t #'gtk-widget-show)) widget))


(defcfun gtk-widget-draw :void (widget pobject) (context :pointer))

(defgeneric draw (widget &optional context)
  (:documentation "context is cl-cairo2 context"))
(defmethod draw ((widget widget) &optional (context cl-cairo2:*context*))
    (cl-cairo2::with-context-pointer (context cntx-pointer)
      (gtk-widget-draw widget cntx-pointer)))

(defcfun gtk-widget-queue-draw-area :void 
  (widget pobject) (x :int) (y :int) (width :int) (height :int))
(defcfun gtk-widget-queue-draw-region :void (widget pobject) (region pobject))
(defcfun gtk-widget-queue-draw :void (widget pobject))

(defgeneric queue-draw (widget &key area region))  
(defmethod queue-draw ((widget widget) &key area region)
  (cond
    (area (apply #'gtk-widget-queue-draw-area widget area))
    (region (gtk-widget-queue-draw-region widget region))
    (t (gtk-widget-queue-draw widget))))

(defcfun gtk-widget-queue-resize :void (widget pobject))
(defcfun gtk-widget-queue-resize-no-redraw :void (widget pobject))

(defgeneric queue-resize (widget &key no-redraw))
(defmethod queue-resize ((widget widget) &key no-redraw)
  (if no-redraw
      (gtk-widget-queue-resize-no-redraw widget)
      (gtk-widget-queue-resize widget)))

(defcfun "gtk_widget_get_size_request" :void
  (widget pobject) (width :pointer) (height :pointer))

(defgeneric size-request (widget))
(defmethod size-request ((widget widget))
  "returns (width height)"
  (with-foreign-outs-list ((width :int) (height :int)) :ignore
      (gtk-widget-get-size-request widget width height)))

(defcfun "gtk_widget_set_size_request"
  :void (widget pobject) (w :int) (h :int))

(defgeneric (setf size-request) (coords widget))
(defmethod (setf size-request) (coords (widget widget))
  "coords = (width height)"
  (gtk-widget-set-size-request widget
                               (first coords)
                               (second coords)))
(save-setter widget size-request)  

(defcfun gtk-widget-intersect :boolean 
  (src1 pobject) (src2 (struct rectangle)) (dest (struct rectangle :out t)))

(defmethod intersect ((rect1 widget) (rect2 rectangle))
   (let ((dest (make-instance 'rectangle)))
     (when (gtk-widget-intersect rect1 rect2 dest)
       dest)))


(defcenum align :fill :start :end :center)

(defbitfield widget-flags
  (:toplevel 16)
  :no-window :realized :mapped :visible :sensitive
  :parent-sensitive :can-focus :set-focus :can-default :has-default
  :has-grab :rc-style :composite-child :no-reparent :app-paintable 
  :recieves-default :double-buffered :no-show-all)

(defgtkslots widget
    name gtk-string
    direction text-direction
    default-direction text-direction
    parent-window pobject
    parent pobject
    child-visible :boolean
    tooltip-markup gtk-string
    tooltip-text gtk-string
    tooltip-window pobject
    has-tooltip :boolean
    can-default :boolean
    can-focus :boolean
    double-buffered :boolean
    has-window :boolean
    visible :boolean
    receives-default :boolean
    mapped :boolean
    realized :boolean
    no-show-all :boolean
    sensitive :boolean
    events event-mask
    visual pobject
    composite-name gtk-string
    halign align
    valign align
    margin-left :int
    margin-right :int
    margin-top :int
    margin-bottom :int
    hexpand :boolean
    hexpand-set :boolean
    vexpand :boolean
    vexpand-set :boolean
    allocation (struct allocation)
    window pobject
    support-multidevice :boolean
    app-paintable :boolean)

(defgtkfuns widget 
  (activate :boolean)
  (hide :boolean)  
  (size-allocate :void (allocation (struct allocation)))
  (add-accelerator :void
   (accel-signal :string) (accel-group pobject) (accel-key key)
   (accel-mods modifier-type) (accel-flags accel-flags))
  (remove-accelerator :boolean
   (accel-group pobject) (accel-key key) (accel-mods modifier-type))
  (list-accel-closures g-list)
  (can-activate-accel :boolean (signal-id :uint))   
  ((widget-event . event) :boolean (event event))
  (send-expose :int (event event))
  (send-focus-change :boolean (event event))
  (is-focus :boolean)
  (grab-focus :void)
  (grab-default :void)
  (override-color :void (state state-flags) (color prgba))
  (override-background-color :void (state state-flags) (color prgba))
  (override-symbolic-color :void (name :string) (color prgba))
  (:get style-context pobject)
  (override-font :void (font pango-cffi:font))
  (:set (widget-accel-path . accel-path) :string 
        (accel-group pobject))
  (destroy :void)
  (render-icon-pixbuf pobject (stock-id :string) (size icon-size))
  (add-events :void (events event-mask))
  (:get device-events event-mask (device pobject))
  (add-device-events :void (device pobject) (events event-mask))
  (:get device-enabled :boolean (device pobject))
  (:get toplevel pobject)
  (:get ancestor pobject (widget-type g-type))
  (is-ancestor :boolean (ancestor pobject))
  ;; region should be cairo_region_t, but it is not realized in cl-cairo2 yet
  (shape-combine-region :void (region pobject))
  (input-shape-combine-region :void (region pobject))
  (:get path (object widget-path))
  (is-composited :boolean)
  (override-cursor :void (cursor prgba) (secondary-cursor prgba))
  (create-pango-context pobject)
  (:get pango-context pobject)
  (create-pango-layout pobject)
  (:set redraw-on-allocate :boolean)
  (mnemonic-activate :boolean (group-cycling :boolean))
  (unparent :void)
  ((widget-map . map) :void) 
  (unmap :void)
  (realize :void)
  (unrealize :void)
  (:get accessible pobject)
  (child-focus :boolean (direction direction-type))
  (child-notify :void (child-property :string))
  (freeze-child-notify :void)
  (:get settings pobject)
  (:get clipboard pobject (selection gatom))
  (:get display pobject)
  (:get root-window pobject)
  (:get screen pobject)
  (has-screen :boolean)
  (thaw-child-notify :void)
  (list-mnemonic-labels g-list-object)
  (add-mnemonic-label :void (label pobject))
  (remove-mnemonic-label :void (label pobject))
  (error-bell :void)
  (keynav-failed :boolean (direction direction-type))
  (trigger-tooltip-query :void)
  (:get allocated-width :int)
  (:get allocated-height :int)
  (is-sensitive :boolean)
  (:get state-flags state-flags)
  (has-default :boolean)
  (has-focus :boolean)
  (has-grab :boolean)
  (is-drawable :boolean)
  (is-toplevel :boolean)
  (device-is-shadowed :boolean (device pobject))
  (reset-style :void)
  (queue-compute-expand :void)
  (compute-expand :boolean (orientation orientation))
  (:set-last device-events event-mask (device pobject))
  (:set-last device-enabled :boolean (device pobject)))

(setf (documentation 'clipboard 'function) 
      "SELECTION should be :PRIMARY or :CLIPOARD")

;; (defcfun gtk-widget-set-device-events :void
;;   (widget pobject) (device pobject) (events event-mask))

;; (defgeneric (setf device-events) (events widget device))
;; (defmethod (setf device-events) (events (widget widget) device)
;;   (gtk-widget-set-device-events widget device events))

;; (defcfun gtk-widget-set-device-enabled :void
;;   (widget pobject) (device pobject) (enabled :boolean))

;; (defgeneric (setf device-enabled) (enable widget device))
;; (defmethod (setf device-enabled) (enabled (widget widget) device)
;;   (gtk-widget-set-device-enabled widget device enabled))

(defcfun ("gtk_widget_pop_composite_child" pop-composite-child) :void)
(defcfun ("gtk_widget_push_composite_child" push-composite-child) :void)

(defcfun gtk-widget-get-pointer :void
  (widget pobject) (x :pointer) (y :pointer))

(defgeneric get-pointer (widget))
(defmethod get-pointer ((widget widget))
  (with-foreign-outs ((x :int) (y :int)) :ignore
    (gtk-widget-get-pointer widget x y)))

(defcfun gtk-widget-translate-coordinates :boolean
  (src-widget pobject) (dst-widget pobject) (src-x :int) (src-y :int)
  (dst-x :pointer) (dst-y :pointer))

(defmethod translate-coordinates ((src-widget widget) (dst-widget widget)
                                  src-x src-y)
  "Returns (values dst-x dst-y)"
  (with-foreign-outs ((dst-x :int) (dst-y :int)) :if-success
    (gtk-widget-translate-coordinates src-widget dst-widget 
                                      src-x src-y dst-x dst-y)))

(defcfun gtk-cairo-should-draw-window :boolean 
  (context :pointer) (gdk-window pobject))

(defgeneric cairo-should-draw-window (window &optional context)
  (:documentation "WINDOW may be GdkWindow or GtkWidget"))
(defmethod cairo-should-draw-window (window 
                                     &optional (context cl-cairo2:*context*))
  (cl-cairo2::with-context-pointer (context cntx-pointer)
      (gtk-cairo-should-draw-window cntx-pointer window)))

(defmethod cairo-should-draw-window ((widget widget)
                                     &optional (context cl-cairo2:*context*))
  (cairo-should-draw-window (window widget) context))

(defcfun gtk-cairo-transform-to-window :void
    (context :pointer) (widget pobject) (gdk-window pobject))

(defgeneric cairo-transform-to-window (widget window &optional context))
(defmethod cairo-transform-to-window ((widget widget) window
                                     &optional (context cl-cairo2:*context*))
  (cl-cairo2::with-context-pointer (context cntx-pointer)
    (gtk-cairo-transform-to-window cntx-pointer widget window)))

(defmethod cairo-transform-to-window ((widget widget) (window widget)
                                     &optional (context cl-cairo2:*context*))
  (cairo-transform-to-window widget (window window) context))

(defcfun gtk-widget-set-state-flags :void 
  (widget pobject) (flags state-flags) (clear :boolean))
(defcfun gtk-widget-unset-state-flags :void 
  (widget pobject) (flags state-flags))

(defgeneric (setf state-flags) (value widget &key type))
(defmethod (setf state-flags) (value (widget widget) &key type)
  "If TYPE = :SET, only set bits, :UNSET -- unset bits, 
otherwise set state = VALUE"
  (case type
    (:set (gtk-widget-set-state-flags widget value nil))
    (:unset (gtk-widget-unset-state-flags widget value))
    (t (gtk-widget-set-state-flags widget value t))))

(defcfun gtk-widget-get-preferred-height :void 
  (widget pobject) (minimum :pointer) (natural :pointer))
(defcfun gtk-widget-get-preferred-height-for-width :void 
  (widget pobject) (width :int) (minimum :pointer) (natural :pointer))

(defgeneric preferred-height (widget &key for-width))
(defmethod preferred-height ((widget widget) &key for-width)
  "Returns (values minimum natural)"
  (with-foreign-outs ((minimum :int) (natural :int)) :ignore
    (if for-width
        (gtk-widget-get-preferred-height-for-width widget
                                                   for-width minimum natural)
        (gtk-widget-get-preferred-height widget minimum natural))))

(defcfun gtk-widget-get-preferred-width :void 
  (widget pobject) (minimum :pointer) (natural :pointer))
(defcfun gtk-widget-get-preferred-width-for-height :void 
  (widget pobject) (height :int) (minimum :pointer) (natural :pointer))

(defgeneric preferred-width (widget &key for-height))
(defmethod preferred-width ((widget widget) &key for-height)
  "Returns (values minimum natural)"
  (with-foreign-outs ((minimum :int) (natural :int)) :ignore
    (if for-height
        (gtk-widget-get-preferred-width-for-height widget 
                                                   for-height minimum natural)
        (gtk-widget-get-preferred-width widget minimum natural))))

(defcenum size-request-mode
  :height-for-width :width-for-height)

(defgtkgetter request-mode size-request-mode widget)

(defcfun gtk-widget-get-preferred-size :void
  (widget pobject) (minimum :pointer) (natural :pointer))

(defgeneric preferred-size (widget))
(defmethod preferred-size ((widget widget))
  "Returns (values minimum natural).
Minimum and natural are requisition objects."
  (with-foreign-outs ((minimum 'requisition) (natural 'requisition))
      :ignore
    (gtk-widget-get-preferred-size widget minimum natural)))

(defcstruct requested-size
  "GtkRequestedSize"
  (data pobject)
  (minimum-size :int)
  (natural-size :int))

(defcfun gtk-distribute-natural-allocation :int
  (extra-space :int) (n-requested-sizes :int) (sizes :pointer))

(defun distribute-natural-allocation (extra-space sizes)
  "EXTRA-SPACE -- integer, extra space to redistribute among children.
SIZES -- {(widget minimum-size natural-size)}*"
  (let ((length (length sizes)))
    (let ((sizes-struct (foreign-alloc 'requested-size
                                       :count length)))
      (iter
        (for i from 0 below length)
        (for x in sizes)
        (let ((el (mem-aref sizes-struct 'requested-size i)))
          (with-foreign-slots ((data minimum-size natural-size) 
                               el requested-size)
            (setf data (first x)
                  minimum-size (second x)
                  natural-size (third x)))))
      (gtk-distribute-natural-allocation extra-space length sizes-struct))))


(init-slots widget nil)

(template
    ((color t)
     (font nil)
     (bg-pixmap nil))
  (destructuring-bind (name with-type) param
    `(progn
       (defmethod ,name ((widget widget) 
                         &key ,@(when with-type '(type)) (state :normal))
         (,name (style-context widget) ,@(when with-type '(:type type)) 
                :state state))
       
       (defmethod (setf ,name) (value (widget widget) 
                                &key ,@(when with-type '(type)) (state :normal))
         (setf (,name (style-context widget) ,@(when with-type '(:type type))
                      :state state)
               value)))))
        

(defclass widget-class (g-object-class)
  ())

(defcstruct widget-class
  (parent-class g-object-class)
  (activate-signal :pointer)
  (dispatch-child-properties-changed :pointer)
  (destroy :pointer)
  (show :pointer)
  (show-all :pointer)
  (hide :pointer)
  (map :pointer)
  (unmap :pointer)
  (realize :pointer)
  (unrealize :pointer)
  (size-allocate :pointer)
  (state-changed :pointer)
  (state-flags-changed :pointer)
  (parent-set :pointer)
  (hierarchy-changed :pointer)
  (style-set :pointer)
  (direction-changed :pointer)
  (grab-notify :pointer)
  (child-notify :pointer)
  (draw :pointer)
  (get-request-mode :pointer)
  (get-preferred-height :pointer)
  (get-preferred-width-for-height :pointer)
  (get-preferred-width :pointer)
  (get-preferred-height-for-width :pointer)
  (mnemonic-activate :pointer)
  (grab-focus :pointer)
  (focus :pointer)
  (move-focus :pointer)
  (keynav-failed :pointer)
  (event :pointer)
  (button-press-event :pointer)
  (button-release-event :pointer)
  (scroll-event :pointer)
  (motion-notify-event :pointer)
  (delete-event :pointer)
  (destroy-event :pointer)
  (key-press-event :pointer)
  (key-release-event :pointer)
  (enter-notify-event :pointer)
  (leave-notify-event :pointer)
  (configure-event :pointer)
  (focus-in-event :pointer)
  (focus-out-event :pointer)
  (map-event :pointer)
  (unmap-event :pointer)
  (property-notify-event :pointer)
  (selection-clear-event :pointer)
  (selection-request-event :pointer)
  (selection-notify-event :pointer)
  (proximity-in-event :pointer)
  (proximity-out-event :pointer)
  (visibility-notify-event :pointer)
  (window-state-event :pointer)
  (damage-event :pointer)
  (grab-broken-event :pointer)
  (selection-get :pointer)
  (selection-received :pointer)
  (drag-begin :pointer)
  (drag-end :pointer)
  (drag-data-get :pointer)
  (drag-data-delete :pointer)
  (drag-leave :pointer)
  (drag-motion :pointer)
  (drag-drop :pointer)
  (drag-data-received :pointer)
  (drag-failed :pointer)
  (popup-menu :pointer)
  (show-help :pointer)
  (get-accessible :pointer)
  (screen-changed :pointer)
  (can-activate-accel :pointer)
  (composited-changed :pointer)
  (query-tooltip :pointer)
  (compute-expand :pointer)
  (adjust-size-request :pointer)
  (adjust-size-allocation :pointer)
  (style-updated :pointer)
  (gtk-reserved :pointer :count 8))

(defgtkfuns widget-class 
  (install-style-property :void (pspec pobject))
  (install-style-property-parser :void (pspec pobject) (parser pfunction))
  (find-style-property (object g-param-spec) (name :string))) 


(defcfun gtk-widget-class-list-style-properties (garray (object g-param-spec))
  (widget-class pobject) (n-properties :pointer))

(defgeneric list-style-properties (widget-class))
(defmethod list-style-properties ((widget-class widget-class))
  (gtk-widget-class-list-style-properties widget-class *array-length*))

(g-object-cffi::generate-property-accessors 
          style-property widget 
          nil gtk-widget-style-get-property
          style-property-type
          widget-class find-style-property %style-properties)

(defgeneric text (widget &key))

