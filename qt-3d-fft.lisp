;;;; qt-3d-fft.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>


;; TODO:
;; Read and write configuration to a text file
;; Save to movie file.

;; Astropilot configuration:
;; a 12.25  (1 2 3 4 5 6 7 15 16 17 18 19)
;; b 5.00  (6 7 8 9 10)
;; h 12.00 (16 16 17 17 18 18 19 19)
;; dt1 1.25 (1 5 11 12 13 14 15)
;; dt2 0.25 (1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)
;; epitrochoid



(in-package #:qt-3d-fft)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 3) (size 0) (debug 3)))

(defparameter *fps* 60)
(defparameter *fft-window-size* 1024)

(defstruct spirograph
  "The parameters used for drawing a spirograph image."
  (steps 244 :type (unsigned-byte 32))
  (a-var (make-animated-var :val 16.0d0 :buckets '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)) :type animated-var)
  (b-var (make-animated-var :val 7.0d0 :buckets '(6 7 8 9 10)) :type animated-var)
  (h-var (make-animated-var :val 9.0d0 :buckets '(16 16 17 17 18 18 19 19)) :type animated-var)
  (dt-1-var (make-animated-var :val 1.15d0 :buckets '(16 16 17 17 18 18 19 19)) :type animated-var)
  (dt-2-var  (make-animated-var :val 0.25d0 :buckets '(1 2 3 4 5 11 12 13 14 15)) :type animated-var)
  (stype :epitrochoid :type t))

(defun reset-spirograph (spiro)
  (with-slots (a-var b-var h-var dt-1-var dt-2-var) spiro
    (reset-var a-var)
    (reset-var b-var)
    (reset-var h-var)
    (reset-var dt-1-var)
    (reset-var dt-2-var)))

;; map-val is used to map logical coordinates to screen coordinates.
(declaim (ftype (cl:function (double-float double-float double-float double-float double-float) double-float) map-val)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) epitrochoid-x)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) epitrochoid-y)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) hypotrochoid-x)
         (ftype (cl:function (double-float double-float double-float double-float) double-float) hypotrochoid-y)
         (inline map-val epitrochoid-x epitrochoid-y hypotrochoid-x hypotrochoid-y))

(defun map-val (x xmin xmax new-xmin new-xmax)
  "Map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (declare (type double-float x xmin xmax new-xmin new-xmax))
  (the double-float (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin)))

(defun epitrochoid-x (a b h tv)
  "X component of the parametric equation for an epitrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (- (* (+ a b) (cos tv)) (* h (cos (* tv (/ (+ a b) b)))))))

(defun epitrochoid-y (a b h tv)
  "Y component of the parametric equation for an epitrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (- (* (+ a b) (sin tv)) (* h (sin (* tv (/ (+ a b) b)))))))

(defun hypotrochoid-x (a b h tv)
  "X component of the parametric equation for an hypotrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (+ (* (- a b) (cos tv)) (* h (cos (* tv (/ (- a b) b)))))))

(defun hypotrochoid-y (a b h tv)
  "Y component of the parametric equation for an hypotrochoid curve."
  (declare (type double-float a b h tv))
  (the double-float (+ (* (- a b) (sin tv)) (* h (sin (* tv (/ (- a b) b)))))))

(define-widget spirograph-animator (QGLWidget)
               ((the-mp3 :initform nil)
                (start-time :initform 0)
                (song-duration :initform 0)
                (window-buffer :initform (make-array *fft-window-size* 
                                           :element-type '(complex double-float)
                                           :adjustable nil
                                           :fill-pointer nil))
                (left-fft-data :initform (make-array *fft-window-size* 
                                           :element-type '(complex double-float)
                                           :adjustable nil
                                           :fill-pointer nil))
                (right-fft-data :initform (make-array *fft-window-size* 
                                            :element-type '(complex double-float)
                                            :adjustable nil
                                            :fill-pointer nil))
                
                (spiro :initform (make-spirograph)))
               (:documentation "The spirograh-drawer widget draws an epitrochoid or hyptrochoid curve using the currently specified parameters."))

(define-subwidget (spirograph-animator timer) (q+:make-qtimer spirograph-animator)
  (setf (q+:single-shot timer) nil))

(define-initializer (spirograph-animator setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background spirograph-animator) nil))

(define-slot (spirograph-animator tick) ()
  (declare (connected timer (timeout)))
  (q+:repaint spirograph-animator))

(define-override (spirograph-animator initialize-G-L) ()
  (gl:clear-color 0 0 0 1)
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (spirograph-animator resize-g-l) (width height)

  (gl:viewport 0 0 width height)
  
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -1.0 1.0 -1.0 1.0 -1.0 1.0))

;; (glu:perspective 50 (/ height width) 1.0 5000.0)
  ;; (glu:look-at 1200 1200 1200 
  ;;              0 0 0
  ;;              0 1 0))

(defun max-radius (spiro)
  (with-slots (a-var b-var h-var) spiro
    (* 1.1d0
       (+ (with-slots (val offset) a-var (+ val offset))
          (with-slots (val offset) b-var (+ val offset))
          (with-slots (val offset) h-var (+ val offset))))))

(define-override (spirograph-animator paint-g-l paint) ()
  "Handle paint events."
  (with-slots (steps a-var b-var h-var dt-1-var dt-2-var stype) spiro

    (let* ((max-radius (max-radius spiro))
           (width (q+:width spirograph-animator))
           (height (q+:height spirograph-animator))
           (x-aspect-ratio (if (< height width)
                               (/ height width 1.0d0)
                               1.0d0))
           (y-aspect-ratio (if (< height width)
                               1.0d0
                               (/ width height 1.0d0)))
           (rdt-1 (+ (+ (animated-var-val dt-1-var)) (animated-var-offset dt-1-var)))
           (location (/ (+ 1 (- (get-internal-real-time) start-time)) 1.0 internal-time-units-per-second))
           (win-center (ceiling (max 0 
                                     (- (* 44100 location)
                                        (round (/ *fft-window-size* 2))))))
           (x-fun (if (eql stype  :epitrochoid) #'epitrochoid-x #'hypotrochoid-x))
           (y-fun (if (eql stype  :epitrochoid) #'epitrochoid-y #'hypotrochoid-y)))

      (with-finalizing 
          ;; Create a painter object to draw on
          ((painter (q+:make-qpainter spirograph-animator)))

        (gl:matrix-mode :modelview)
        (gl:load-identity)

        (gl:clear :color-buffer :depth-buffer)
        ;; Clear the background
        (when (and the-mp3 (< (/ (- (get-internal-real-time) start-time) 1.0 internal-time-units-per-second) song-duration))

          (bordeaux-fft:windowed-fft! (mp3-file-left-channel the-mp3)
                                      window-buffer left-fft-data
                                      win-center *fft-window-size* 'bordeaux-fft:triangle)
          (bordeaux-fft:windowed-fft! (mp3-file-right-channel the-mp3) 
                                      window-buffer right-fft-data
                                      win-center *fft-window-size* 'bordeaux-fft:triangle)
          
          ;; Actual drawing goes here.  In this case, just a line.
          (gl:push-matrix)


          ;; TODO: Use "modern" OpenGL
          (gl:with-primitives :lines
            (gl:color 1 0 0)
            (loop
               for i below steps
               for cur-t = 0.0d0 then (* i rdt-1)
               do
                 (let ((r-a (with-slots (val offset) a-var (+ val offset)))
                       (r-b (with-slots (val offset) b-var (+ val offset)))
                       (r-h (with-slots (val offset) h-var (+ val offset)))
                       (r-dt-2 (with-slots (val offset) dt-2-var (+ val offset))))

                   (gl:vertex (map-val (* x-aspect-ratio (funcall x-fun  r-a r-b r-h cur-t))
                                       (- max-radius) max-radius
                                       -1.0d0 1.0d0)
                              (map-val (* y-aspect-ratio (funcall y-fun r-a r-b r-h cur-t))
                                       (- max-radius) max-radius
                                       -1.0d0 1.0d0)
                              0.0d0)
                   (gl:vertex (map-val (* x-aspect-ratio (funcall x-fun r-a r-b r-h (+ r-dt-2 cur-t)))
                                       (- max-radius) max-radius
                                       -1.0d0 1.0d0)
                              (map-val (* y-aspect-ratio (funcall y-fun r-a r-b r-h (+ r-dt-2 cur-t)))
                                       (- max-radius) max-radius
                                       -1.0d0 1.0d0)
                              0.0d0))))

        ;; Update offsets based on fft-data
        (step-var a-var left-fft-data right-fft-data)
        (step-var b-var left-fft-data right-fft-data)
        (step-var h-var left-fft-data right-fft-data)
        (step-var dt-1-var left-fft-data right-fft-data)
        (step-var dt-2-var left-fft-data right-fft-data)
        (gl:pop-matrix))))))

(define-widget spirograph-widget (QWidget)
               ()
               (:documentation "A Spirograph animator and its controls."))

(define-subwidget (spirograph-widget fft-viewer) (make-instance 'spirograph-animator)
  "The spirograph-drawer itself.")


;; TODO: Use macros or something to make this less repetitive

(define-subwidget (spirograph-widget mp3-file-edit) (q+:make-qlineedit spirograph-widget)
  "The currently open file."
  (setf (q+:read-only mp3-file-edit) t))

(defun format-list (lst)
  (format nil "~{~d~^ ~}" lst))

  

(define-subwidget (spirograph-widget a-val-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'a' value spinbox."
  (q+:set-decimals a-val-spin 2)
  (q+:set-single-step a-val-spin 0.25)
  (q+:set-maximum a-val-spin 10000.0)
  (q+:set-minimum a-val-spin 0.0)
  (q+:set-value a-val-spin (animated-var-val (spirograph-a-var (slot-value fft-viewer 'spiro)))))

(define-subwidget (spirograph-widget a-buckets-edit) (q+:make-qlineedit spirograph-widget)
  (setf (q+:text a-buckets-edit) (format-list (animated-var-buckets
                                               (spirograph-a-var (slot-value fft-viewer 'spiro))))))

(define-subwidget (spirograph-widget b-val-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'b' value spinbox."
  (q+:set-decimals b-val-spin 2)
  (q+:set-single-step b-val-spin 0.25)
  (q+:set-maximum b-val-spin 10000.0)
  (q+:set-minimum b-val-spin 0.0)
  (q+:set-value b-val-spin (animated-var-val (spirograph-b-var (slot-value fft-viewer 'spiro)))))

(define-subwidget (spirograph-widget b-buckets-edit) (q+:make-qlineedit spirograph-widget)
  (setf (q+:text b-buckets-edit) (format-list (animated-var-buckets
                                               (spirograph-b-var (slot-value fft-viewer 'spiro))))))


(define-subwidget (spirograph-widget h-val-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'h' value spinbox."
  (q+:set-decimals h-val-spin 2)
  (q+:set-single-step h-val-spin 0.25)
  (q+:set-maximum h-val-spin 10000.0)
  (q+:set-minimum h-val-spin 0.0)
  (q+:set-value h-val-spin (animated-var-val (spirograph-h-var (slot-value fft-viewer 'spiro)))))

(define-subwidget (spirograph-widget h-buckets-edit) (q+:make-qlineedit spirograph-widget)
  (setf (q+:text h-buckets-edit) (format-list (animated-var-buckets
                                               (spirograph-h-var (slot-value fft-viewer 'spiro))))))

(define-subwidget (spirograph-widget dt-1-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'dt-1' value spinbox."
  (q+:set-decimals dt-1-spin 5)
  (q+:set-single-step dt-1-spin 0.01)
  (q+:set-minimum dt-1-spin 0.0)
  (q+:set-maximum dt-1-spin (* pi 1000))
  (q+:set-value dt-1-spin (animated-var-val (spirograph-dt-1-var (slot-value fft-viewer 'spiro)))))

(define-subwidget (spirograph-widget dt-1-buckets-edit) (q+:make-qlineedit spirograph-widget)
  (setf (q+:text dt-1-buckets-edit) (format-list (animated-var-buckets
                                               (spirograph-dt-1-var (slot-value fft-viewer 'spiro))))))

(define-subwidget (spirograph-widget dt-2-spin) (q+:make-qdoublespinbox spirograph-widget)
  "The 'dt-2' value spinbox."
  (q+:set-decimals dt-2-spin 5)
  (q+:set-single-step dt-2-spin 0.01)
  (q+:set-minimum dt-2-spin 0.0)
  (q+:set-maximum dt-2-spin (* pi 1000))
  (q+:set-value dt-2-spin (animated-var-val (spirograph-dt-2-var (slot-value fft-viewer 'spiro)))))

(define-subwidget (spirograph-widget dt-2-buckets-edit) (q+:make-qlineedit spirograph-widget)
  (setf (q+:text dt-2-buckets-edit) (format-list (animated-var-buckets
                                               (spirograph-dt-2-var (slot-value fft-viewer 'spiro))))))


(define-subwidget (spirograph-widget epitrochoid-button) (q+:make-qradiobutton "Epitrochoid")
  "Epitrochoid radio button."
  (q+:set-checked epitrochoid-button t))

(define-subwidget (spirograph-widget hypotrochoid-button) (q+:make-qradiobutton "Hypotrochoid")
  "Hypotrochoid radio button.")

(define-subwidget (spirograph-widget button-group) (q+:make-qbuttongroup spirograph-widget)
  "Button group to ensure radio buttons are exclusive."
  (q+:set-exclusive button-group t)
  (q+:add-button button-group epitrochoid-button)
  (q+:add-button button-group hypotrochoid-button))

(define-subwidget (spirograph-widget steps-spin) (q+:make-qspinbox spirograph-widget)
  "The spinbox for the number of steps."
  (q+:set-maximum steps-spin 10000000)
  (q+:set-minimum steps-spin 4)
  (q+:set-value steps-spin (spirograph-steps (slot-value fft-viewer 'spiro))))


(define-subwidget (spirograph-widget reset-button) (q+:make-qpushbutton "Reset" spirograph-widget)
  "Reset all-offsets to 0")

(define-slot (spirograph-widget type-changed) ()
  "Handle radio button changes that hcange the curve type."
  (declare (connected epitrochoid-button (released)))
  (declare (connected hypotrochoid-button (released)))
  (cond 
    ;; Epitrochoid selected
    ((q+:is-checked epitrochoid-button)
     (setf (spirograph-stype (slot-value fft-viewer 'spiro)) :epitrochoid))

    ;; Hypotrochoid selected
    ((q+:is-checked hypotrochoid-button)
     (setf (spirograph-stype (slot-value fft-viewer 'spiro)) :hypotrochoid))
    
    ;; One of the above should always be true, but just in case...
    ;; Print a warning and toggle the  epitrochoid-button
    (t
     (format t "Warning: No type selected, defaulting to epitrochoid.~%")
     (q+:set-checked epitrochoid-button t)
     (setf (spirograph-stype (slot-value fft-viewer 'spiro)) :epitrochoid)))

  ;; Repaint to reflect the changes
  (q+:repaint fft-viewer))

(define-slot (spirograph-widget editing-finished) ()
  (declare (connected a-buckets-edit (editing-finished void)))
  (declare (connected b-buckets-edit (editing-finished void)))
  (declare (connected h-buckets-edit (editing-finished void)))
  (declare (connected dt-1-buckets-edit (editing-finished void)))
  (declare (connected dt-2-buckets-edit (editing-finished void)))

  (with-slots (a-var b-var h-var dt-1-var dt-2-var) (slot-value fft-viewer 'spiro)
    (setf (animated-var-buckets a-var) (read-from-string (format nil "( ~a )" (q+:text a-buckets-edit))))
    (setf (animated-var-buckets b-var) (read-from-string (format nil "( ~a )" (q+:text b-buckets-edit))))
    (setf (animated-var-buckets h-var) (read-from-string (format nil "( ~a )" (q+:text h-buckets-edit))))
    (setf (animated-var-buckets dt-1-var) (read-from-string (format nil "( ~a )" (q+:text dt-1-buckets-edit))))
    (setf (animated-var-buckets dt-2-var) (read-from-string (format nil "( ~a )" (q+:text dt-2-buckets-edit))))

    (q+:repaint fft-viewer)))

(define-slot (spirograph-widget steps-changed) ((value int))
  "Handle changes to the steps-spin box."
  (declare (connected steps-spin (value-changed int)))
  (setf (spirograph-steps (slot-value fft-viewer 'spiro)) (q+:value steps-spin))
  (q+:repaint fft-viewer))

(define-slot (spirograph-widget values-changed) ((value double))
  "Handle changes to all of the spin boxes except steps."
  (declare (connected a-val-spin (value-changed double)))
  (declare (connected b-val-spin (value-changed double)))
  (declare (connected h-val-spin (value-changed double)))
  (declare (connected dt-1-spin (value-changed double)))
  (declare (connected dt-2-spin (value-changed double)))
  
  (with-slots (a-var b-var h-var dt-1-var dt-2-var) (slot-value fft-viewer 'spiro)
    (setf (animated-var-val a-var) (q+:value a-val-spin))
    (setf (animated-var-val b-var) (q+:value b-val-spin))
    (setf (animated-var-val h-var) (q+:value h-val-spin))
    (setf (animated-var-val dt-1-var) (q+:value dt-1-spin))
    (setf (animated-var-val dt-2-var) (q+:value dt-2-spin))
    (q+:repaint fft-viewer)))

(define-subwidget (spirograph-widget control-layout) (q+:make-qvboxlayout spirograph-widget)
  "Layout all of the control widgets in a vertical box layout."

  ;; Create horizontal layouts to hold the labels and spinboxes
  (let ((file-layout (q+:make-qhboxlayout))
        (a-layout (q+:make-qhboxlayout))
        (b-layout (q+:make-qhboxlayout))
        (h-layout (q+:make-qhboxlayout))
        (dt-1-layout (q+:make-qhboxlayout))
        (dt-2-layout (q+:make-qhboxlayout))
        (other-layout (q+:make-qhboxlayout)))
    
    ;; Populate the horizontal layouts and add them to the top level vertical layout

    (q+:add-widget file-layout (q+:make-qlabel "Filename: " spirograph-widget))
    (q+:add-widget file-layout mp3-file-edit)

    (q+:add-widget a-layout (q+:make-qlabel "A: " spirograph-widget))
    (q+:add-widget a-layout a-val-spin)
    (q+:add-widget a-layout a-buckets-edit)

    (q+:add-widget b-layout (q+:make-qlabel "B: " spirograph-widget))
    (q+:add-widget b-layout b-val-spin)
    (q+:add-widget b-layout b-buckets-edit)

    (q+:add-widget h-layout (q+:make-qlabel "H: " spirograph-widget))
    (q+:add-widget h-layout h-val-spin)
    (q+:add-widget h-layout h-buckets-edit)

    (q+:add-widget dt-1-layout (q+:make-qlabel "dt1: " spirograph-widget))
    (q+:add-widget dt-1-layout dt-1-spin)
    (q+:add-widget dt-1-layout dt-1-buckets-edit)

    (q+:add-widget dt-2-layout (q+:make-qlabel "dt2: " spirograph-widget))
    (q+:add-widget dt-2-layout dt-2-spin)
    (q+:add-widget dt-2-layout dt-2-buckets-edit)

    (q+:add-widget other-layout (q+:make-qlabel "steps: " spirograph-widget))
    (q+:add-widget other-layout steps-spin)

    ;; Add the radio buttons directly to the vertical layout
    (q+:add-widget other-layout epitrochoid-button)
    (q+:add-widget other-layout hypotrochoid-button)

    (q+:add-widget other-layout reset-button)

    (q+:add-layout control-layout file-layout)
    (q+:add-layout control-layout a-layout)
    (q+:add-layout control-layout b-layout)
    (q+:add-layout control-layout h-layout)
    (q+:add-layout control-layout dt-1-layout)
    (q+:add-layout control-layout dt-2-layout)
    (q+:add-layout control-layout other-layout)

    ;; Finally add the spirograph viewer directly to the vertical layout
    (q+:add-widget control-layout fft-viewer)))


(define-signal (spirograph-widget open-mp3) (string))

(define-slot (spirograph-widget reset-pressed) ()
  "Handle the reset button."
  (declare (connected reset-button (released)))
  (reset-spirograph (slot-value fft-viewer 'spiro)))

(define-slot (spirograph-widget open-mp3) ((file-name string))
  (declare (connected spirograph-widget (open-mp3 string)))
  (let* ((new-mp3-file (read-mp3-file file-name))
         (sduration (mp3-file-duration-in-seconds new-mp3-file))
         (tframes (ceiling (* sduration *fps*))))

    (setf (q+:text mp3-file-edit) file-name)
    (reset-spirograph (slot-value fft-viewer 'spiro))
    (setf (slot-value fft-viewer 'the-mp3) new-mp3-file)
    (setf (slot-value fft-viewer 'song-duration) sduration)
    (setf (slot-value fft-viewer 'start-time) (get-internal-real-time))))

;; (define-slot set-spiro (spirograph-widget new-spiro)
;;   (setf (slot-value spiro-widget 'spiro) new-spiro)
;;   (with-slots (a-var b-var h-var dt-1-var dt-2-var steps stype) new-spiro
;;     (q+:set-value steps-spin steps)
;;     (if (eql stype  :epitrochoid)
;;         (q+:set-checked epitrochoid-button t)
;;         (q+:set-checked hypotrochoid-button t))
    
;;     (with-slots (val buckets) a-var
;;       (q+:set-value a-spin val)
;;       (q+:set-text a-buckets-edit) (format-list buckets))

;;     (with-slots (val buckets) b-var
;;       (q+:set-value b-spin val)
;;       (q+:set-text b-buckets-edit) (format-list buckets))

;;     (with-slots (val buckets) h-var
;;       (q+:set-value h-spin val)
;;       (q+:set-text h-buckets-edit) (format-list buckets))

;;     (with-slots (val buckets) dt-1-var
;;       (q+:set-value dt-1-spin val)
;;       (q+:set-text dt-1-buckets-edit) (format-list buckets))

;;     (with-slots (val buckets) dt-2-var
;;       (q+:set-value dt-2-spin val)
;;       (q+:set-text dt-2-buckets-edit) (format-list buckets))))

(define-slot (spirograph-widget open-config) ((file-name string))
  (declare (connected spirograph-widget (open-config string)))
  (with-open-file (str file-name :direction :input)
    (with-standard-io-syntax 
      (let ((new-spiro (read str)))
        (reset-spirograph new-spiro)
        (with-slots (a-var b-var h-var dt-1-var dt-2-var steps stype) new-spiro
          (q+:set-value steps-spin steps)
          (if (eql stype  :epitrochoid)
              (q+:set-checked epitrochoid-button t)
              (q+:set-checked hypotrochoid-button t))
          
          (with-slots (val buckets) a-var
            (q+:set-value a-val-spin val)
            (q+:set-text a-buckets-edit (format-list buckets)))

          (with-slots (val buckets) b-var
            (q+:set-value b-val-spin val)
            (q+:set-text b-buckets-edit (format-list buckets)))

          (with-slots (val buckets) h-var
            (q+:set-value h-val-spin val)
            (q+:set-text h-buckets-edit (format-list buckets)))

          (with-slots (val buckets) dt-1-var
            (q+:set-value dt-1-spin val)
            (q+:set-text dt-1-buckets-edit (format-list buckets)))

          (with-slots (val buckets) dt-2-var
            (q+:set-value dt-2-spin val)
            (q+:set-text dt-2-buckets-edit (format-list buckets))))))))
        ;; (set-spiro fft-viewer new-spiro)))))
  ;;     (setf (slot-value fft-viewer 'spiro) (read str))))
  ;; (reset-spirograph (slot-value fft-viewer 'spiro))
  ;; (set-spiro spirograph-widget (slot-value fft-viewer 'spiro))

(define-slot (spirograph-widget save-config) ((file-name string))
  (declare (connected spirograph-widget (save-config string)))
  (with-open-file (str file-name :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (with-standard-io-syntax 
      (print (slot-value fft-viewer 'spiro) str))))

(define-widget main-window (QMainWindow)
  ((mixer :initform (mixalot:create-mixer))
   (current-stream :initform nil)))

(define-override (main-window close-event) (ev)
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Open MP3" (ctrl o))
         (open-mp3 main-window))
  (:separator)
  (:item ("Open Config" (ctrl i))
         (open-config main-window))
  (:item ("Save Config" (ctrl s))
         (save-config main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))

(define-subwidget (main-window spirograph-viewer) (make-instance 'spirograph-widget)
  "The central spirograph-widget.")


(define-slot (main-window open open-mp3) ()
  (let ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.music-location))
                                                     "*.mp3")))
    (when (and filename (> (length filename) 0))
      (signal! spirograph-viewer (open-mp3 string) filename)
      (when current-stream (mixalot:mixer-remove-streamer mixer current-stream))
      (setf current-stream (mixalot-mp3:make-mp3-streamer filename))
      (mixalot:mixer-add-streamer mixer current-stream))))

(define-slot (main-window openc open-config) ()
  (let ((filename (q+:qfiledialog-get-open-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.home-location))
                                                     "*.txt")))
    (when (and filename (> (length filename) 0))
      (signal! spirograph-viewer (open-config string) filename))))

(define-slot (main-window save save-config) ()
  (let ((filename (q+:qfiledialog-get-save-file-name main-window "Select File"
                                                     (q+:qdesktopservices-storage-location 
                                                      (q+:qdesktopservices.home-location))
                                                     "*.txt")))
    (when (and filename (> (length filename) 0))
      (signal! spirograph-viewer (save-config string) filename))))

(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) spirograph-viewer))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))
