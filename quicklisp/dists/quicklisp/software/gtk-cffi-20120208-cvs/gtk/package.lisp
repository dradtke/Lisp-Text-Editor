;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for gtk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage gtk-cffi
  (:use #:common-lisp #:cffi #:alexandria #:iterate
        #:cffi-object #:g-object-cffi #:g-lib-cffi #:gdk-cffi 
        #:gtk-cffi-utils)
  (:shadow #:image #:window)
  (:export
   ;;;; common
   #:gtk-init
   #:gtk-main
   #:gtk-main-quit
   #:gtk-model
   #:defmodel  ; recommended way

   ;; reexport
   #:object-by-id
   #:gsignal
   #:yield

   #:css-provider
   #:css-provider-load
   
   #:widget
   ;; widget slots
   #:name
   #:direction
   #:default-direction
   #:parent-window
   #:has-tooltip
   #:can-focus
   #:double-buffered
   #:events
   #:visual
   #:composite-name
   #:halign
   #:valign
   #:margin-left
   #:margin-right
   #:margin-top
   #:margin-bottom
   #:hexpand
   #:hexpand-set
   #:vexpand
   #:vexpand-set
   #:app-paintable
   #:size-request
   #:direction
   #:default-direction
   #:color
   #:font
   #:parent-window
   #:state
   #:bg-pixmap
   #:allocation
   #:parent
   #:child-visible
   #:tooltip-markup
   #:tooltip-text
   #:tooltip-window
   #:can-default
   #:has-window
   #:visible
   #:receives-default
   #:mapped
   #:realized
   #:no-show-all
   #:colormap
   #:sensitive
   #:widget-accel-path
   #:style-context
   #:device-events
   #:device-enabled
   #:toplevel
   #:ancestor
   #:is-ancestor
   #:path
   #:is-composited
   #:pango-context
   #:redraw-on-allocate ; setter only
   #:accessible
   #:settings
   #:clipboard
   #:display
   #:root-window
   #:screen
   #:has-screen
   #:allocated-width
   #:allocated-height
   #:is-sensitive
   #:is-focus
   #:state-flags
   #:has-default
   #:has-focus
   #:has-grab
   #:is-drawable
   #:is-toplevel
   #:device-is-shadowed
   #:preferred-height
   #:preferred-width
   #:preferred-size
   ;; methods
   #:activate
   #:show
   #:hide
   #:draw
   #:queue-draw
   #:queue-resize
   #:size-allocate
   #:add-accelerator
   #:remove-accelerator
   #:list-accel-closures
   #:can-activate-accel
   #:widget-event
   #:send-expose
   #:send-focus-change
   #:intersect
   #:grab-focus
   #:grab-default
   #:override-color
   #:override-background-color
   #:override-symbolic-color
   #:override-font
   #:override-cursor
   #:render-icon-pixbuf
   #:add-events
   #:get-pointer
   #:translate-coordinates
   #:shape-combine-region
   #:input-shape-combine-region
   #:create-pango-context
   #:create-pango-layout
   #:mnemonic-activate
   #:widget-map
   #:unmap
   #:realize
   #:unrealize
   #:child-focus
   #:child-notify
   #:freeze-child-notify
   #:thaw-child-notify
   #:destroy
   #:list-mnemonic-labels
   #:add-mnemonic-label
   #:remove-mnemonic-label
   #:error-bell
   #:keynav-failed
   #:trigger-tooltip-query
   #:reset-style
   #:queue-compute-expand
   #:compute-expand
 
   #:pop-composite-child
   #:push-composite-child
   #:cairo-should-draw-window
   #:cairo-transform-to-window
   #:distribute-natural-allocation

   #:widget-class
   #:install-style-property
   #:install-style-property-parser
   #:list-style-properties
   #:find-style-property
   #:style-property

   #:bin
   ;; methods
   #:child
   
   #:container
   ;; container slots
   #:border-width
   ;; methods
   #:add
   #:propagate-expose
   
   #:window
   ;; window slots
   #:default-size
   #:screen
   #:transient-for
   #:window-position
   ;; methods

   #:dialog
   ;;methods
   #:run
   #:response
   #:add-button
   #:default-response
   #:add-action-widget
   #:response-sensitive
   #:response-for-widget
   #:action-area
   #:content-area
   #:alternative-button-order
   #:alternative-dialog-button-order

   #:entry
   ;; entry slots
   #:text
   #:visibility
   #:max-length
   #:entry-buffer
   #:activates-default
   #:has-frame
   #:inner-border
   #:width-chars
   #:alignment
   #:overwrite-mode
   #:completion
   #:cursor-hadjustment
   #:progress-fraction
   #:progress-pulse-step

  
   #:button
   
   #:box
   ;; box methods
   #:pack
   #:pack*

   #:v-box

   #:h-box

   #:button-box

   #:h-button-box

   #:event-box

   #:cell-renderer

   #:cell-renderer-text

   #:cell-renderer-toggle
   
   #:cell-renderer-pixbuf

   #:cell-layout

   #:cell-editable
   
   #:misc
   ;; misc slots
   #:alignment

   #:label
   ;; label slots
   #:text
   #:mnemonic-widget
   #:justify

   #:with-markup

   #:paned
   #:h-paned
   #:v-paned

   #:frame
   ;; frame slots
   #:shadow-type

   #:tree-model
   ;; tree-model slots
   #:columns
   ;; tree-model methods
   #:tree-model-foreach
   #:get-index
   #:with-tree-iter
   #:n-columns
   #:column-type

   #:%iter
   #:tree-iter
   #:iter->path
   #:path->iter
   #:get-indices
   
   #:list-store
   ;; list-store methods
   #:model-values
   #:append-values
   #:append-iter
   #:clear

   #:tree-model-filter
   ;; tree-model-filter slots
   #:visible-column
   #:with-child-path

   #:tree-view
   ;; tree-view slots
   #:model
   #:search-column
   ;; tree-view methods
   #:append-column
   #:get-selection
   #:path-at-pos
   #:with-path-at-pos
   #:%path
   #:column
   #:get-cursor
   #:with-get-cursor-path

   #:tree-view-column
   ;; tree-view-column slots
   #:sort-column-id
   #:alignment
   #:reorderable
   ;; tree-view-column methods
   #:cell-data-func
   #:cell-get-position
   #:cell-renderers
   #:get-cell-at
   
   #:scrolled-window
   ;; scrolled-window slots
   #:hadjustment
   #:vadjustment
   #:shadow-type
   #:placement
   #:min-content-width
   #:min-content-height
   #:policy
   ;; scrolled-window methods
   #:unset-placement
   #:add-with-viewport

   #:tree-selection
   ;; tree-selection methods
   #:mode
   #:with-selection
   #:get-selected

   #:text-iter
   ;; slots
   #:line
   #:offset
   #:line-offset
   #:line-index
   #:visible-line-index
   #:visible-line-offset
   ;; methods
   #:text-iter-char
   #:slice
   #:text-iter-text
   #:visible-slice
   #:visible-text
   #:pixbuf
   #:marks
   #:toggled-tags
   #:child-anchor
   #:begins-tag
   #:ends-tag
   #:toggles-tag
   #:has-tag
   #:tags
   #:text-iter-editable
   #:can-insert
   #:starts-word
   #:ends-word
   #:inside-word
   #:starts-line
   #:starts-sentence
   #:ends-sentence
   #:inside-sentence
   #:is-cursor-position
   #:chars-in-line
   #:bytes-in-line
   #:get-attributes
   #:language
   #:is-end
   #:is-start
   #:forward-char
   #:backward-char
   #:forward-chars
   #:backward-chars
   #:forward-line
   #:backward-line
   #:forward-lines
   #:backward-lines
   #:forward-word-end
   #:backward-word-start
   #:forward-word-ends
   #:backward-word-starts
   #:forward-cursor-position
   #:backward-cursor-position
   #:forward-cursor-positions
   #:backward-cursor-positions
   #:backward-sentence-start
   #:forward-sentence-end
   #:backward-sentence-starts
   #:forward-sentence-ends
   #:forward-visible-word-end
   #:backward-visible-word-start
   #:forward-visible-word-ends
   #:backward-visible-word-starts
   #:forward-visible-cursor-position
   #:backward-visible-cursor-position
   #:forward-visible-cursor-positions
   #:backward-visible-cursor-positions
   #:forward-visible-line
   #:backward-visible-line
   #:forward-visible-lines
   #:backward-visible-lines
   #:forward-to-end
   #:forward-to-line-end
   #:forward-to-tag-toggle
   #:backward-to-tag-toggle
   #:forward-search
   #:backward-search
   #:text-iter-equal
   #:compare
   #:in-range
   #:order
   #:forward-find-char
   #:backward-find-char

   #:text-buffer
   ;; slot
   #:modified
   ;; methods
   #:line-count
   #:char-count
   #:tag-table
   #:insert-pixbuf
   #:insert-child-anchor
   #:create-child-anchor
   #:create-mark
   #:add-mark
   #:mark
   #:get-insert
   #:selection-bound
   #:has-selection
   #:place-cursor
   #:select-range
   #:remove-all-tags
   #:delete-selection
   #:paste-clipboard
   #:copy-clipboard
   #:cut-clipboard
   #:begin-user-action
   #:end-user-action
   #:add-selection-clipboard
   #:remove-selection-clipboard
   #:deserialize-can-create-tags
   #:copy-target-list
   #:paste-target-list
   #:register-deserialize-tagset
   #:register-serialize-tagset
   #:unregister-deserialize-format
   #:unregister-serialize-format
   #:start-iter
   #:end-iter 
   #:text
   #:insert
   #:insert-range
   #:text-buffer-delete
   #:backspace
   #:text-buffer-slice
   #:move-mark
   #:delete-mark
   #:apply-tag
   #:remove-tag
   #:create-tag
   #:text-buffer-iter
   #:bounds
   #:selection-bounds
   #:deserialize
   #:deserialize-formats
   #:serialize
   #:serialize-formats
   #:register-serialize-format
   #:register-deserialize-format
   
   #:text-view
   ;; slots
   #:buffer
   #:wrap-mode
   #:editable
   #:cursor-visible
   #:overwrite
   #:pixels-above-lines
   #:pixels-below-lines
   #:pixels-inside-wrap
   #:justification
   #:left-margin
   #:right-margin
   #:indent
   #:tabs
   #:accepts-tab
   ;; methods
   #:scroll-to-mark
   #:scroll-to-iter
   #:scroll-mark-onscreen
   #:move-mark-onscreen
   #:place-cursor-onscreen
   #:text-view-window
   #:window-type
   #:border-window-size
   #:forward-display-line
   #:backward-display-line
   #:forward-display-line-end
   #:backward-display-line-start
   #:starts-display-line
   #:move-visually
   #:add-child-at-anchor
   #:add-child-in-window
   #:move-child
   #:default-attributes
   #:im-context-filter-keypress
   #:reset-im-context

   #:text-child-anchor
   #:widgets

   #:text-tag
   #:priority
   #:event
   
   #:appearance
   #:direction
   #:text-attributes-font
   #:font-scale
   #:language
   #:invisible
   #:bg-full-height
   #:editable
   #:bg-color
   #:fg-color
   #:rise
   #:underline
   #:strikethrough
   #:draw-bg
   #:inside-selection
   #:is-text

   #:combo-box
   #:append-text
   #:prepend-text
   #:insert-text
   #:remove-text
   #:active-text

   #:message-dialog

   ;; handy defun
   #:show-message 

   #:file-chooser
   ;; file-chooser slots
   #:filename
   
   #:file-chooser-dialog

   #:file-chooser-button
   
   #:progress-bar
   ;; progress-bar slots
   #:fraction

   #:table
   ;; table methods
   #:attach
   #:resize

   #:menu-shell

   #:menu

   #:menu-bar
   #:pack-direction
   #:child-pack-direction

   #:menu-item
   #:right-justified
   #:use-underline
   #:submenu
   #:accel-path

   #:tool-shell

   #:toolbar

   #:notebook

   #:statusbar
   #:context-id
   #:statusbar-push
   #:statusbar-pop
   #:statusbar-remove
   #:message-area

   #:icon-source

   #:image

   #:expander

   #:application
   ))

(in-package #:gtk-cffi)
(register-package "Gtk" *package*)
(register-prefix *package* 'gtk)